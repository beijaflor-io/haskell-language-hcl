{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HCLSpec where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.HCL
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe
import           Test.Hspec
import           Text.Megaparsec        (runParser)

fs' = unsafePerformIO $ do
    fs <- liftIO (getDirectoryContents "./test-fixtures")
    return $ filter ((== ".hcl") . takeExtension) fs

testParser p i o = case runParser p "" i of
    Left e -> error (show e)
    Right a -> (a `shouldBe` o)

testFailure fp inp = case parseHCL fp inp of
    Right _ -> error "This should have failed"
    _ -> True `shouldBe` True

spec :: Spec
spec = do
    describe "stringParts" $ do
        it "parses normal strings" $ do
            let input = "\"something\""
            testParser stringParts input [HCLStringPlain "something"]

        it "parses interpolated strings" $ do
            let input = "\"${asdf} Hello World asdfasdf ${hey}\""
            testParser stringParts input [ HCLStringInterp "asdf"
                                         , HCLStringPlain " Hello World asdfasdf "
                                         , HCLStringInterp "hey"
                                         ]

    describe "ident" $ do
        it "parses alphanum" $ do
            let input = "asdf"
            testParser ident input "asdf"

        it "parses dashes" $ do
            let input = "asdf-asdf"
            testParser ident input "asdf-asdf"

        it "parses underscores" $ do
            let input = "asdf_asdf"
            testParser ident input "asdf_asdf"

        it "stops at whitespace" $ do
            let input = "asdf asdf"
            testParser ident input "asdf"

    describe "stringPlain" $ do
        it "parses the empty string" $ do
            let input = ""
            testParser stringPlain input ""

        it "parses charaters" $ do
            let input = "something"
            testParser stringPlain input "something"

        it "parses escape sequences" $ do
            let input = "bar\\\"baz\\n"
            testParser stringPlain input "bar\"baz\n"

    describe "stringPlainMultiline" $ do
        it "parses multiline strings" $ do
            let input = Text.unlines [ "<<EOF"
                                     , "stuff"
                                     , "here"
                                     , "EOF"
                                     ]
            testParser stringPlainMultiline input "stuff\nhere"

    describe "assignment" $ do
        it "parses weird string assignments (escaping)" $ do
            let input = "foo = \"bar\\\"\\n\""
            testParser assignment input (["foo"], bplain "bar\"\n")

        it "parses HashiCorp's escape.hcl" $ do
            input <- Text.readFile "./test-fixtures/escape.hcl"
            testParser assignment input (["foo"], bplain "bar\"baz\\n")

        it "parses string assignments" $ do
            let input = "foo = \"bar\""
            testParser assignment input (["foo"], bplain "bar")

        it "parses assignments to numbers" $ do
            let input = "foo = 10"
            testParser assignment input (["foo"], HCLNumber 10)

        it "parses assignments to lists" $ do
            let input = "foo = [1, 2, 3]"
            testParser assignment input
                (["foo"], HCLList [HCLNumber 1, HCLNumber 2, HCLNumber 3])

        it "parses multiline string assignments" $ do
            let input = Text.unlines $ [ "bar = <<EOF"
                                       , "hello there"
                                       , "here"
                                       , "EOF"
                                       ]
            testParser assignment input
                (["bar"], bplain "hello there\nhere")

        it "parses nested assignments" $ do
            let input = "foo.bar = 10"
            testParser assignment input
                (["foo", "bar"], HCLNumber 10)

        it "parses assignments to objects" $ do
            let input = "foo = { name = \"john\" }"
            testParser assignment input
                (["foo"], HCLObject [] [(["name"], bplain "john")])

    describe "parseHCL" $ do
        it "parses basic assignments" $ do
            let input = Text.pack $ unlines $ ["foo = \"bar\"", "bar = \"foo\""]
            testParser hcl input
                [ (HCLStatementAssignment (["foo"], bplain "bar"))
                , (HCLStatementAssignment (["bar"], bplain "foo"))
                ]

        it "parses multiline string assignments" $ do
            let input = Text.pack $ unlines $ [ "bar = <<EOF"
                                              , "hello there"
                                              , "here"
                                              , "EOF"
                                              ]
            testParser hcl input
                [ (HCLStatementAssignment (["bar"], bplain "hello there\nhere"))
                ]

        it "parses interpolated assignments" $ do
            let input = Text.pack $ unlines $ [ "foo = \"bar\""
                                              , "bar = \"${file(\"bing/bong.txt\")}\""
                                              ]
            testParser hcl input
                [ (HCLStatementAssignment (["foo"], bplain "bar"))
                , (HCLStatementAssignment (["bar"], binterp "file(\"bing/bong.txt\")"))
                ]

        it "parses complex interpolated assignments" $ do
            let input = Text.pack $ unlines $ [ "foo = \"bar\""
                                              , "bar = \"stuff/${file(\"bing/bong.txt\")}\""
                                              ]
            testParser hcl input
                [ (HCLStatementAssignment (["foo"], bplain "bar"))
                , (HCLStatementAssignment (["bar"], HCLString [ HCLStringPlain "stuff/"
                                                            , HCLStringInterp "file(\"bing/bong.txt\")"
                                                            ]))
                ]

    describe "Hashicorp Test Suite" $ forM_ fs' $ \fp -> it fp $ do
        inp <- liftIO $ Text.readFile ("test-fixtures" </> fp)
        case fp of
            "unterminated_block_comment.hcl" -> testFailure fp inp
            "multiline_no_marker.hcl" -> testFailure fp inp
            "multiline_bad.hcl" -> testFailure fp inp
            "unterminated_brace.hcl" -> testFailure fp inp
            _ -> case parseHCL fp inp of
                    Left e -> error (show e)
                    Right _ -> True `shouldBe` True
