{-# LANGUAGE FlexibleContexts #-}
module Data.HCL.TestHelper where

import           Data.HCL
import           Data.Text            (Text)
import           Test.Hspec
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- testParser :: Parser Text -> Text -> Text -> Expectation
-- testParser p i o = case runParser p "" i of
--     Left e -> error (show e)
--     Right a -> a `shouldBe` o

-- testFailure :: FilePath -> Parser a -> Text -> Expectation
-- testFailure fp p i = case runParser p fp i of
--     Right _ -> error "This should have failed"
--     _ -> True `shouldBe` True

testParser
    :: (Eq a, Show e, Show a, Show (Token s))
    => Parsec e s a -> s -> a -> Expectation
testParser p i o = case runParser p "" i of
    Left e  -> error (show e)
    Right a -> a `shouldBe` o

testFailure :: String -> Text -> Expectation
testFailure fp inp = case parseHCL fp inp of
    Right _ -> error "This should have failed"
    _       -> True `shouldBe` True

testFailureP :: Parser Text -> String -> Text -> Expectation
testFailureP p fp inp = case runParser p fp inp of
    Right _ -> error "This should have failed"
    _       -> True `shouldBe` True
