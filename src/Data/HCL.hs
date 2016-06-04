{-# LANGUAGE OverloadedStrings #-}
module Data.HCL where

import           Control.Monad
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap (fromList)
import           Data.Scientific       (Scientific)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import           Text.Megaparsec       (Dec, ParseError (..), alphaNumChar,
                                        char, eol, many, manyTill, optional,
                                        runParser, sepBy, skipMany, some,
                                        spaceChar, tab, try, (<|>))
import qualified Text.Megaparsec       as Megaparsec (string)
import qualified Text.Megaparsec.Lexer as Lexer
import           Text.Megaparsec.Text  (Parser)

import           Data.HCL.LexChar

-- type HCLObject = HashMap Text HCLValue
type HCLList = [HCLValue]

data HCLValue = HCLNumber Scientific
              | HCLString Text
              | HCLIdent Text
              | HCLObject [Text] (HashMap Text HCLValue)
              | HCLList [HCLValue]
  deriving(Show, Eq)

-- data HCLKObject =
--     HCLKObject [Text] (HashMap Text HCLValue)
--   deriving(Show)
type HCLDoc = [HCLStatement]
data HCLStatement = HCLStatementObject HCLValue
                  | HCLStatementAssignment (Text, HCLValue)

-- parseHCL :: FilePath -> Text -> Either ParseError HCLValue
parseHCL :: String -> Text -> Either
    (ParseError Char Dec) HCLDoc
parseHCL = runParser $
    many $ do
        skipSpace
        topValue

topValue =
    HCLStatementObject <$> try object
    <|> HCLStatementAssignment <$> assignment

value :: Parser HCLValue
value =
    try object
    <|> HCLList <$> list
    <|> number
    <|> HCLIdent <$> ident
    <|> HCLString <$> string

object :: Parser HCLValue
object = do
    ks <- keys
    skipSpace
    vchar '{'
    skipSpace
    fs <- manyTill assignment (vchar '}')
    skipSpace
    return $ HCLObject ks $ HashMap.fromList fs

keys :: Parser [Text]
keys = many $ do
    k <- key
    skipSpace
    return k

assignment :: Parser (Text, HCLValue)
assignment = do
    i <- ident
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
    optional comma
    skipSpace
    vchar ']'
    return vs

comma :: Parser ()
comma =
    vchar ','

quote :: Parser ()
quote =
    vchar '"'

string :: Parser Text
string = try mstr <|> str
  where
    mstr = do
        Megaparsec.string "<<"
        optional (char '-')
        Megaparsec.string "EOF"
        eol
        Text.pack <$> manyTill Lexer.charLiteral
            (many spaceChar >> Megaparsec.string "EOF")
    str = do
        quote
        s <- manyTill Lexer.charLiteral quote
        return $ Text.pack s

number :: Parser HCLValue
number =
    HCLNumber <$> Lexer.number

ident :: Parser Text
ident = Text.pack <$> some (alphaNumChar <|> char '_')


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

-- hclValueParser = do
--     ident <- hclIdentParser
--     hclWhiteParser
--     val <- hclLiteralParser
--     return (ident, val)

-- hclIdentParser = alphaNumChar

-- hclLiteralParser = return ()

-- spec = do
--     fs <- getDirectoryContents "./test-fixtures"
--     let fs' = filter ((== ".hcl") . takeExtension) fs
--     forM_ fs' $ \fp -> do
--         putStrLn fp
--         inp <- Text.readFile ("./test-fixtures" </> fp)
--         print $ parseHCL fp inp
--         putStrLn "--------------------------------------------------------------------------------"
