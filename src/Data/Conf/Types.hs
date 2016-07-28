module Data.Conf.Types
  where

import           Data.Text            (Text)

type Conf = [ConfStatement]

data ConfStatement = ConfStatementComment Comment
                   | ConfStatementBlock Block
                   | ConfStatementEmptyLine
                   -- ^ We store empty lines while parsing so we can
                   -- reconstruct the document when pretty-printing
                   | ConfStatementExpression Expression
  deriving(Eq, Show)

data Block = Block [Text] [ConfStatement]
  deriving(Eq, Show)

data Comment = Comment Text
  deriving(Eq, Show)

data Expression = Expression Text [Text]
  deriving(Eq, Show)
