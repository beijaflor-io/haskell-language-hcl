{-|
Module: Data.HCL
Description: Exports a @.hcl@ Megaparsec parser through 'hcl'
Copyright: (c) Copyright Pedro Tacla Yamada 2016
License: MIT
Maintainer: tacla.yamada@gmail.com
Stability: experimental
Portability: unknown

This modules contains the 'hcl' Megaparsec parser for @hcl@ files.

The pretty-printer is at "Data.HCL.PrettyPrint".
-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HCL
    (
      -- * Entry-points
      parseHCL
    , hcl
    , runParser
      -- * Types
    , HCLDoc (..)
    , HCLStatement (..)
    , HCLValue (..)
    , HCLList (..)
    , HCLStringPart (..)
      -- * Pretty-printer
    , Pretty (..)
      -- * Support functions
    , topValue
    , bplain
    , binterp
    , string
    , stringParts
    , stringPart
    , stringPlain
    , stringPlainMultiline
    , stringInterp
    , assignment
    , object
    , value
    , ident
    , keys
    , key
    , list
    , number
    , Parser
    )
  where

import           Control.Monad
import qualified Data.HashMap.Strict        as HashMap (fromList)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Prettyprint.Doc  (Pretty)
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParseError (..), Parsec, eof,
                                             label, lookAhead, many, manyTill,
                                             optional, runParser, sepBy, sepBy1,
                                             skipMany, some, try, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, anyChar, char, eol,
                                             spaceChar, tab)
import qualified Text.Megaparsec.Char       as Megaparsec (string)
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Data.HCL.Types


type Parser = Parsec Void Text



-- |
-- Parser for the HCL format
--
-- @
-- let h = runParser hcl fileName fileContents
-- @
--
-- See "Text.Megaparsec"
hcl :: Parser HCLDoc
hcl = many $ do
    skipSpace
    topValue

-- |
-- Shortcut for @runParser 'hcl'@
parseHCL :: String -> Text -> Either (ParseError Char Void) HCLDoc
parseHCL = runParser hcl

topValue :: Parser HCLStatement
topValue = label "HCL - topValue" $
    HCLStatementObject <$> try object
    <|> HCLStatementAssignment <$> assignment

value :: Parser HCLValue
value = label "HCL - value" $
    try object
    <|> HCLList <$> list
    <|> number
    <|> HCLIdent <$> ident
    <|> HCLString <$> stringParts
    <|> HCLString <$> (do s <- stringPlainMultiline; return [HCLStringPlain s])

object :: Parser HCLValue
object = label "HCL - object" $ do
    ks <- keys
    skipSpace
    vchar '{'
    skipSpace
    fs <- manyTill assignment (vchar '}')
    skipSpace
    return $ HCLObject ks $ HashMap.fromList fs

keys :: Parser [Text]
keys = label "HCL - keys" $ many $ do
    k <- key
    skipSpace
    return k

assignment :: Parser ([Text], HCLValue)
assignment = label "HCL - assignment" $ do
    i <- sepBy1 ident (char '.')
    skipSpace
    vchar '='
    skipSpace
    v <- value
    skipSpace
    return (i, v)

vchar :: Char -> Parser ()
vchar =
    void . char

key :: Parser Text
key = string <|> ident

list :: Parser HCLList
list = do
    vchar '['
    skipSpace
    vs <- value `sepBy` (skipSpace >> comma >> skipSpace)
    skipSpace
    _ <- optional comma
    skipSpace
    vchar ']'
    return vs

comma :: Parser ()
comma =
    vchar ','

-- quote :: Parser ()
quote :: Parser Text
quote = Lexer.symbol skipSpace "\""

bplain :: Text -> HCLValue
bplain s = HCLString [HCLStringPlain s]

binterp :: Text -> HCLValue
binterp s = HCLString [HCLStringInterp s]

stringParts :: Parser [HCLStringPart]
stringParts = label "HCL - stringParts" $ do
    _ <- quote
    manyTill stringPart quote

stringPart :: Parser HCLStringPart
stringPart = label "HCL - stringPart" $
    try (HCLStringInterp <$> stringInterp)
    <|> HCLStringPlain <$> stringPlain

stringInterp :: Parser Text
stringInterp = label "HCL - stringInterp" $ do
    _ <- Lexer.symbol skipSpace "${"
    Text.pack <$> manyTill anyChar (Megaparsec.string "}")

stringPlain :: Parser Text
stringPlain = label "HCL - stringPlain" $ do
    let end =
            try (lookAhead eof)
            <|> void (try (lookAhead (Megaparsec.string "${")))
            <|> void (try (lookAhead quote))
    s <- manyTill Lexer.charLiteral end
    return $ Text.pack s

stringPlainMultiline :: Parser Text
stringPlainMultiline = label "HCL - stringPlainMultiline" $ do
    _ <- Megaparsec.string "<<"
    _ <- optional (char '-')
    _ <- Megaparsec.string "EOF"
    _ <- eol
    Text.pack <$> manyTill Lexer.charLiteral
        (try (skipSpace >> Megaparsec.string "EOF"))

string :: Parser Text
string = label "HCL - string" $ try stringPlainMultiline <|> str
  where
    str = do
        _ <- quote
        s <- manyTill Lexer.charLiteral quote
        return $ Text.pack s

number :: Parser HCLValue
number =
    HCLNumber <$> Lexer.scientific

ident :: Parser Text
ident = Text.pack <$> some (alphaNumChar <|> char '_' <|> char '-')

skipSpace :: Parser ()
skipSpace = skipMany $
    skipLineComment
    <|> skipBlockComment
    <|> void eol
    <|> void spaceChar
    <|> void tab

skipLineComment :: Parser ()
skipLineComment =
    Lexer.skipLineComment "#"
    <|> Lexer.skipLineComment "//"

skipBlockComment :: Parser ()
skipBlockComment =
    Lexer.skipBlockComment "/*" "*/"
