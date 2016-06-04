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

    describe "string" $
        it "parses escape sequences" $ do
            let input = "\"bar\\\"baz\\n\""
            output <- testParser string input "bar\"baz\n"
            print output

    describe "assignment" $ do
        it "parses weird string assignments (escaping)" $ do
            let input = "foo = \"bar\\\"\\n\""
            testParser assignment input ("foo", HCLString "bar\"\n")

        it "parses HashiCorp's escape.hcl" $ do
            input <- Text.readFile "./test-fixtures/escape.hcl"
            testParser assignment input ("foo", HCLString "bar\"baz\\n")

        it "parses string assignments" $ do
            let input = "foo = \"bar\""
            testParser assignment input ("foo", HCLString "bar")

        it "parses assignments to numbers" $ do
            let input = "foo = 10"
            testParser assignment input ("foo", HCLNumber 10)

        it "parses assignments to lists" $ do
            let input = "foo = [1, 2, 3]"
            testParser assignment input
                ("foo", HCLList [HCLNumber 1, HCLNumber 2, HCLNumber 3])

        it "parses assignments to objects" $ do
            let input = "foo = { name = \"john\" }"
            testParser assignment input
                ("foo", HCLObject [] [("name", HCLString "john")])

    -- describe "parseHCL" $
    --     it "parses basic assignments" $
    --     let input = Text.pack $ unlines $ ["foo = \"bar\"", "bar = \"foo\""]
    --     in case parseHCL "" input of
    --            Left e -> error (show e)
    --            Right a -> 10 `shouldBe` 10

--     fs <- getDirectoryContents "./test-fixtures"
--     let fs' = filter ((== ".hcl") . takeExtension) fs
--     forM_ fs' $ \fp -> do
--         putStrLn fp
--         inp <- Text.readFile ("./test-fixtures" </> fp)
--         print $ parseHCL fp inp
--         putStrLn "--------------------------------------------------------------------------------"


