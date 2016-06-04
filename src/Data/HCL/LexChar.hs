{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.HCL.LexChar
  where

import           Control.Applicative        (optional, some, (<|>))
import           Control.Monad              (void)
import           Data.Char                  (lexLitChar)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe                 (fromMaybe, isJust, listToMaybe)
import           Data.Scientific            (Scientific, toRealFloat)
import qualified Text.Megaparsec.Char       as C
import           Text.Megaparsec.Combinator
import           Text.Megaparsec.Error
import           Text.Megaparsec.Pos
import           Text.Megaparsec.Prim

lexChar :: (MonadParsec e s m, Token s ~ Char) => m String
lexChar = label "lexChar" $ do
    -- The @~@ is needed to avoid requiring a MonadFail constraint,
    -- and we do know that r will be non-empty if count' succeeds.
    ~r@(x:_) <- lookAhead $ count' 1 8 C.anyChar
    case listToMaybe (lexLitChar r) of
        Just (c,r') -> count (length r - length r') C.anyChar >> return c
        Nothing -> unexpected (Tokens (x :| []))
