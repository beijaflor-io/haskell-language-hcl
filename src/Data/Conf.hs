{-# LANGUAGE OverloadedStrings #-}
module Data.Conf
  where

import           Data.Either
import           Data.Maybe           (catMaybes, fromMaybe)

import           Control.Monad        (void)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Text.Megaparsec
import           Text.Megaparsec.Text

type Conf = [ConfStatement]

data ConfStatement = ConfStatementComment Comment
                   | ConfStatementBlock Block
                   | ConfStatementEmptyLine
                   | ConfStatementExpression Expression
  deriving(Eq, Show)

data Block = Block [Text] [ConfStatement]
  deriving(Eq, Show)

data Comment = Comment Text
  deriving(Eq, Show)

data Expression = Expression Text [Text]
  deriving(Eq, Show)

conf :: Parser Conf
conf = label "conf" $
    postProcessStatementLines <$> manyTill confStatementLines eof

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

-- testExpression = do
--     let r = runParser expression "" "worker_processes 1;"
--     print r
-- testConf = do
--     let r = runParser conf "" "worker_processes 1;\nsomething 2;"
--     print r
-- testConf2 = do
--     let r = runParser conf "" "location / {\nalias /something;\n}"
--     print r
-- {-# NOINLINE testFile #-}
-- testFile :: String
-- testFile = unsafePerformIO $ readFile "/usr/local/etc/nginx/nginx.conf"
-- parseConf fp inp = runParser conf fp inp
-- test = do
--     let r = parseConf "" (Text.pack testFile)
--     print r
    -- [ ConfStatementExpression (Expression "worker_processes" ["1"])
    -- , ConfStatementBlock (Block
    --                        ["events"]
    --                        [ ConfStatementExpression (Expression "worker_connections" ["1024"])
    --                        ])
    -- , ConfStatementBlock (Block
    --                        ["http"]
    --                        [ ConfStatementExpression (Expression "include" ["mime.types"])
    --                        , ConfStatementExpression (Expression "default_type" ["application/octet-stream"]
    --                                                  )
    --                        , ConfStatementExpression (Expression "sendfile" ["on"])
    --                        , ConfStatementExpression (Expression "keepalive_timeout" ["65"])
    --                        , ConfStatementBlock (Block
    --                                               ["server"]
    --                                               [ ConfStatementExpression (Expression "listen" ["9898"])
    --                                               , ConfStatementExpression (Expression "server_name" ["localhost"])
    --                                               , ConfStatementBlock (Block
    --                                                                      ["location", "/"]
    --                                                                      [ ConfStatementExpression (Expression "root" ["/usr/local/Library/Taps/railwaycat/homebrew-emacsmacport"])
    --                                                                      , ConfStatementExpression (Expression "index" ["index.html index.htm"])
    --                                                                      ])
    --                                               , ConfStatementExpression (Expression "error_page" ["500 502 503 504  /50x.html"])
    --                                               , ConfStatementBlock (Block
    --                                                                      ["location" , "=" , "/50x.html"]
    --                                                                      [ ConfStatementExpression (Expression "root" ["html"])
    --                                                                      ])
    --                                               ])
    --                        , ConfStatementExpression (Expression "include" ["servers/*"])
    --                        ])
    -- ]
