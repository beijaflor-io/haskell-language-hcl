module Data.Conf.Internal
  where

import           Control.Monad        (void)
import           Data.Conf.Types
import           Data.Either
import           Data.Maybe           (catMaybes, fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Text.Megaparsec
import           Text.Megaparsec.Text

-- | The .conf parser
--
-- See "Text.Megaparsec"
conf :: Parser Conf
conf = label "conf" $
    postProcessStatementLines <$> manyTill confStatementLines eof

-- ** Support functions
skipSpace :: Parser ()
skipSpace = void $ many spaceChar

confStatement :: Parser ConfStatement
confStatement = label "confStatement" $
    try (ConfStatementComment <$> comment)
    <|> try (ConfStatementBlock <$> block)
    <|> ConfStatementExpression <$> expression

confStatementLines :: Parser [ConfStatement]
confStatementLines = do
    skipSpace
    s <- confStatement
    -- If we're after a comment, the parser consumed a EOL. Anywhere else we
    -- should look for two ends of lines repeated (the end of the current
    -- expression, plus an empty one)
    me <- optional $ try $ case s of
        ConfStatementComment _ -> eol >> return ConfStatementEmptyLine
        _ -> eol >> eol >> return ConfStatementEmptyLine
    skipSpace
    return $ catMaybes [Just s, me]

postProcessStatementLines :: [[ConfStatement]] -> [ConfStatement]
postProcessStatementLines = removeTrailing . concat
  where
    removeTrailing [] = []
    removeTrailing xs | last xs == ConfStatementEmptyLine = init xs
                      | otherwise = xs

comment :: Parser Comment
comment = label "comment" $ do
    _ <- string "#"
    -- Comments always start with a space
    c <- fromMaybe ' ' <$> optional (char ' ')
    Comment . Text.pack . (c:) <$> manyTill anyChar eol

block :: Parser Block
block = label "block" $ do
    s <- flip someTill (char '{') $ do
        k <- Text.pack <$>
            some (noneOf ['{', ' ', '}', ';'])
        skipSpace
        return k
    skipSpace
    Block s . postProcessStatementLines <$>
        manyTill confStatementLines (char '}')

expression :: Parser Expression
expression = label "expression" $ do
    s <- manyTill (letterChar <|> char '_' <|> char '-') spaceChar
    skipSpace
    as <- manyTill argument (char ';')
    return $ Expression (Text.pack s) as

argument :: Parser Text
argument = label "argument" $
    Text.pack <$> some (noneOf [';'])
