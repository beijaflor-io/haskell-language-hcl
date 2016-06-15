{-# LANGUAGE OverloadedStrings #-}
module Data.ConfSpec where

import           Data.Conf
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

spec :: Spec
spec = do
    describe "comment" $
        it "parses a comment" $ do
            let inp = Text.unlines [ "# something here"
                                   , ""
                                   , "# there"
                                   ]
            parse comment "" inp `shouldParse` Comment " something here"

    describe "expression" $ do
        it "parses an expression" $ do
            let inp = Text.unlines [ "worker_processes 1;"]
            parse expression "" inp `shouldParse` Expression "worker_processes" ["1"]

        it "parses a single-line expression" $ do
            let inp = "worker_processes 1;"
            parse expression "" inp `shouldParse` Expression "worker_processes" ["1"]

    describe "block" $ do
        it "parses a block" $ do
            let inp = Text.unlines [ "http {"
                                   , "  listen 8989;"
                                   , "  proxy_pass something;"
                                   , "}"
                                   ]
            parse block "" inp `shouldParse` Block ["http"]
                                                   [ ConfStatementExpression (Expression "listen" ["8989"])
                                                   , ConfStatementExpression (Expression "proxy_pass" ["something"])
                                                   ]

        it "parses a nested block" $ do
            let inp = Text.unlines [ "http {"
                                   , "  location / {"
                                   , "    proxy_pass something;"
                                   , "  }"
                                   , "}"
                                   ]
            parse block "" inp `shouldParse` Block ["http"]
                                                   [ ConfStatementBlock (Block
                                                     ["location", "/"]
                                                     [ConfStatementExpression (Expression "proxy_pass" ["something"])])
                                                   ]

        it "parses empty lines" $ do
            let inp = Text.unlines [ "http {"
                                   , "  listen 8989;"
                                   , ""
                                   , "  proxy_pass something;"
                                   , "}"
                                   ]
            parse block "" inp `shouldParse` Block ["http"]
                                                   [ ConfStatementExpression (Expression "listen" ["8989"])
                                                   , ConfStatementEmptyLine
                                                   , ConfStatementExpression (Expression "proxy_pass" ["something"])
                                                   ]

        it "parses skips more than one empty line" $ do
            let inp = Text.unlines [ "http {"
                                   , "  listen 8989;"
                                   , ""
                                   , ""
                                   , "  proxy_pass something;"
                                   , "}"
                                   ]
            parse block "" inp `shouldParse` Block ["http"]
                                                   [ ConfStatementExpression (Expression "listen" ["8989"])
                                                   , ConfStatementEmptyLine
                                                   , ConfStatementExpression (Expression "proxy_pass" ["something"])
                                                   ]

        it "ignores one trailing line" $ do
            let inp = Text.unlines [ "http {"
                                   , "  listen 8989;"
                                   , ""
                                   , ""
                                   , "  proxy_pass something;"
                                   , "}"
                                   , ""
                                   ]
            parse block "" inp `shouldParse` Block ["http"]
                                                   [ ConfStatementExpression (Expression "listen" ["8989"])
                                                   , ConfStatementEmptyLine
                                                   , ConfStatementExpression (Expression "proxy_pass" ["something"])
                                                   ]

        it "ignores multiple trailing lines" $ do
            let inp = Text.unlines [ "http {"
                                   , "  listen 8989;"
                                   , ""
                                   , ""
                                   , "  proxy_pass something;"
                                   , "}"
                                   , ""
                                   , ""
                                   , ""
                                   ]
            parse block "" inp `shouldParse` Block ["http"]
                                                   [ ConfStatementExpression (Expression "listen" ["8989"])
                                                   , ConfStatementEmptyLine
                                                   , ConfStatementExpression (Expression "proxy_pass" ["something"])
                                                   ]

    describe "conf" $ do
        it "parses nginx configuration successfully" $ do
            inp <- Text.readFile "default.conf"
            parse conf "" `shouldSucceedOn` inp

        it "ignores one leading line" $ do
            let inp = Text.unlines [ ""
                                   , "http {"
                                   , "  listen 8989;"
                                   , ""
                                   , ""
                                   , "  proxy_pass something;"
                                   , "}"
                                   , ""
                                   , ""
                                   , ""
                                   ]
            parse conf "" inp `shouldParse` [ ConfStatementBlock $ Block
                                                ["http"]
                                                [ ConfStatementExpression (Expression "listen" ["8989"])
                                                , ConfStatementEmptyLine
                                                , ConfStatementExpression (Expression "proxy_pass" ["something"])
                                                ]
                                            ]

        it "ignores multiple leading lines" $ do
            let inp = Text.unlines [ ""
                                   , ""
                                   , "http {"
                                   , "  listen 8989;"
                                   , ""
                                   , ""
                                   , "  proxy_pass something;"
                                   , "}"
                                   , ""
                                   , ""
                                   , ""
                                   ]
            parse conf "" inp `shouldParse` [ ConfStatementBlock $ Block
                                                ["http"]
                                                [ ConfStatementExpression (Expression "listen" ["8989"])
                                                , ConfStatementEmptyLine
                                                , ConfStatementExpression (Expression "proxy_pass" ["something"])
                                                ]
                                            ]

        it "ignores multiple trailing lines" $ do
            let inp = Text.unlines [ "http {"
                                   , "  listen 8989;"
                                   , ""
                                   , ""
                                   , "  proxy_pass something;"
                                   , "}"
                                   , ""
                                   , ""
                                   , ""
                                   ]
            parse conf "" inp `shouldParse` [ ConfStatementBlock $ Block
                                                ["http"]
                                                [ ConfStatementExpression (Expression "listen" ["8989"])
                                                , ConfStatementEmptyLine
                                                , ConfStatementExpression (Expression "proxy_pass" ["something"])
                                                ]
                                            ]

        it "parses comments" $ do
            let inp = Text.unlines [ " # something"
                                   , ""
                                   , "http {"
                                   , "  listen 8989;"
                                   , ""
                                   , ""
                                   , "  proxy_pass something;"
                                   , "}"
                                   , ""
                                   , ""
                                   , ""
                                   ]
            parse conf "" inp `shouldParse` [ ConfStatementComment $ Comment " something"
                                            , ConfStatementEmptyLine
                                            , ConfStatementBlock $ Block
                                                ["http"]
                                                [ ConfStatementExpression (Expression "listen" ["8989"])
                                                , ConfStatementEmptyLine
                                                , ConfStatementExpression (Expression "proxy_pass" ["something"])
                                                ]
                                            ]
