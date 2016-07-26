{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HCL.Class where

import           Control.Arrow
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import           Data.HCL
-- import           Data.HCL.PrettyPrint
-- import           Data.HCL.Types
import           Data.Monoid
import           Data.Text           (Text)
-- import           Text.PrettyPrint.HughesPJClass (pPrint)

class ToHCLDoc a where
    toHCLDoc :: a -> HCLDoc

class ToHCLValue a where
    toHCLValue :: a -> HCLValue

instance ToHCLValue HCLStringPart where
    toHCLValue h = HCLString [h]

instance ToHCLValue [HCLStringPart] where
    toHCLValue = HCLString

instance ToHCLValue Text where
    toHCLValue h = HCLString [HCLStringPlain h]

reduceTarget :: ToHCLValue a => [([Text], a)] -> HashMap [Text] HCLValue
reduceTarget m = HashMap.fromList (map (second toHCLValue) m)

provider :: ToHCLValue a => [Text] -> [([Text], a)] -> HCLDoc
provider ts m = [ HCLStatementObject $ HCLObject ("provider":ts) $
                  reduceTarget m
                ]

resource :: [Text] -> [([Text], HCLValue)] -> HCLDoc
resource ts m = [ HCLStatementObject $ HCLObject ("resource":ts) $
                  HashMap.fromList m
                ]

var :: Text -> [HCLStringPart]
var v = [HCLStringInterp v]

str :: Text -> [HCLStringPart]
str v = [HCLStringPlain v]

dsl :: HCLDoc
dsl =
    provider ["datadog"] [ (["api_key"], str "${var.datadog_api_key}")
                         , (["app_key"], str "${var.datadog_app_key}")
                         ]
    <> resource ["datadog_monitor", "default"] []
