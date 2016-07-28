{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.HCL.Types where

import           Control.DeepSeq     (NFData)
import           Data.HashMap.Strict (HashMap)
import           Data.Scientific     (Scientific)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

-- | The HCL document is just a list of statements
type HCLDoc = [HCLStatement]

-- | Statements may be "objects", of form:
--
-- @
-- provider "aws" {
-- # more
-- }
-- @
--
-- Or they may be assignments:
--
-- @
-- a = "b"
-- @
data HCLStatement = HCLStatementObject HCLValue
                  | HCLStatementAssignment ([Text], HCLValue)
  deriving(Generic, Show, Eq, NFData)

data HCLValue = HCLNumber Scientific
              | HCLString [HCLStringPart]
              | HCLIdent Text
              | HCLObject [Text] (HashMap [Text] HCLValue)
              | HCLList [HCLValue]
  deriving(Generic, Show, Eq, NFData)

type HCLList = [HCLValue]

data HCLStringPart = HCLStringPlain Text
                   | HCLStringInterp Text
  deriving(Generic, Show, Eq, NFData)
