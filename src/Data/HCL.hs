{-# LANGUAGE OverloadedStrings #-}
module Data.HCL where

import           Control.Monad
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import           Data.Monoid ((<>))
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Text.Megaparsec       (Dec, ParseError (..), alphaNumChar, anyChar,
                                        between, char, eof, eol, many, manyTill, optional,
                                        runParser, sepBy, skipMany, some,
                                        spaceChar, tab, lookAhead, try, (<|>))
import qualified Text.Megaparsec as Megaparsec (string)
import qualified Text.Megaparsec.Lexer as Lexer
import           Text.Megaparsec.Text (Parser)

import           Data.HCL.LexChar

-- type HCLObject = HashMap Text HCLValue
type HCLList = [HCLValue]

data HCLStringPart = HCLStringPlain Text
                   | HCLStringInterp Text
  deriving(Show, Eq)

data HCLValue = HCLNumber Scientific
              | HCLString [HCLStringPart]
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
  deriving(Show, Eq)

-- parseHCL :: FilePath -> Text -> Either ParseError HCLValue
parseHCL :: String -> Text -> Either
    (ParseError Char Dec) HCLDoc
parseHCL = runParser hcl

hcl = many $ do
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
    <|> HCLString <$> stringParts

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

-- quote :: Parser ()
quote :: Parser String
quote = Lexer.symbol skipSpace "\""

bplain s = HCLString [HCLStringPlain s]
binterp s = HCLString [HCLStringInterp s]

stringParts :: Parser [HCLStringPart]
stringParts = do
    quote
    manyTill stringPart quote

stringPart :: Parser HCLStringPart
stringPart =
    try (HCLStringInterp <$> stringInterp)
    <|> HCLStringPlain <$> stringPlain

stringInterp :: Parser Text
stringInterp = do
    Lexer.symbol skipSpace "${"
    Text.pack <$> manyTill anyChar (Megaparsec.string "}")

stringPlain :: Parser Text
stringPlain = do
    let end =
            try (lookAhead eof)
            <|> void (try (lookAhead (Megaparsec.string "${")))
            <|> void (try (lookAhead quote))
    s <- manyTill Lexer.charLiteral end
    return $ Text.pack s

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
