module Data.HCL.Types where

import           Data.HashMap.Strict (HashMap)
import           Data.Scientific (Scientific)
import           Data.Text (Text)

type HCLList = [HCLValue]

data HCLStringPart = HCLStringPlain Text
                   | HCLStringInterp Text
  deriving(Show, Eq)

data HCLValue = HCLNumber Scientific
              | HCLString [HCLStringPart]
              | HCLIdent Text
              | HCLObject [Text] (HashMap [Text] HCLValue)
              | HCLList [HCLValue]
  deriving(Show, Eq)

type HCLDoc = [HCLStatement]
data HCLStatement = HCLStatementObject HCLValue
                  | HCLStatementAssignment ([Text], HCLValue)
  deriving(Show, Eq)
