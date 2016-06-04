{-# LANGUAGE OverloadedStrings #-}
module Data.HCL where

import           System.Directory
import           System.FilePath

import           Control.Monad
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap (fromList)
import           Data.Scientific       (Scientific)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import           Text.Megaparsec       (Dec, ParseError (..), alphaNumChar,
                                        char, eol, many, manyTill, runParser,
                                        sepBy, skipMany, some, spaceChar, tab,
                                        (<|>))
import qualified Text.Megaparsec.Lexer as Lexer
import           Text.Megaparsec.Text  (Parser)

type HCLObject = HashMap Text HCLValue
type HCLList = [HCLValue]

data HCLValue = HCLNumber Scientific
              | HCLString Text
              | HCLIdent Text
              | HCLObject HCLObject
              | HCLList [HCLValue]
  deriving(Show)

data HCLKObject =
    HCLKObject [Text] (HashMap Text HCLValue)
  deriving(Show)
type HCLDoc = [HCLKObject]

-- parseHCL :: FilePath -> Text -> Either ParseError HCLValue
parseHCL :: String -> Text -> Either
    (ParseError Char Dec) HCLDoc
parseHCL = runParser $
    many $ do
        skipSpace
        kobject

value :: Parser HCLValue
value =
    HCLObject <$> object
    <|> HCLList <$> list
    <|> HCLIdent <$> ident
    <|> number
    <|> HCLString <$> string

object :: Parser HCLObject
object = do
    vchar '{'
    skipSpace
    fs <- manyTill assignment (vchar '}')
    skipSpace
    return $ HashMap.fromList fs

kobject :: Parser HCLKObject
kobject = do
    ks <- keys
    skipSpace
    h <- object
    return $ HCLKObject ks h

keys :: Parser [Text]
keys = some $ do
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
    vs <- value `sepBy` comma
    vchar ']'
    return vs

comma :: Parser ()
comma =
    vchar ','

quote :: Parser ()
quote =
    vchar '"'

string :: Parser Text
string = do
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

spec = do
    -- fs <- getDirectoryContents "./data"
    let fs' = ["complex.hcl"] -- filter ((== ".hcl") . takeExtension) fs
    forM_ fs' $ \fp -> do
        putStrLn fp
        inp <- Text.readFile ("./data" </> fp)
        print $ parseHCL fp inp
        putStrLn "--------------------------------------------------------------------------------"
