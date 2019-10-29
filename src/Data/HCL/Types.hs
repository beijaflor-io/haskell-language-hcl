{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.HCL.Types where

import           Control.DeepSeq           (NFData)
import           Data.HashMap.Strict       (HashMap, toList)
import           Data.Monoid               ((<>))
import           Data.Scientific           (Scientific, toRealFloat)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc (Doc, Pretty, align, annotate, comma,
                                            defaultLayoutOptions, dot, dquotes,
                                            encloseSep, fill, hsep,
                                            layoutPretty, nest, pretty,
                                            prettyList, punctuate, unAnnotate,
                                            vsep, (<+>))
import           GHC.Generics              (Generic)




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



instance Pretty HCLStatement where
    pretty s = case s of
        HCLStatementObject o           -> pretty o
        HCLStatementAssignment (is, v) ->
          encloseSep "" "" dot (pretty <$> is) <+> "=" <+> pretty v
    prettyList = vsep . fmap pretty

instance Pretty HCLValue where
    pretty v = case v of
        HCLNumber n    -> pretty $ (toRealFloat n :: Double)
        HCLString ps   -> dquotes $ hsep $ pretty <$> ps
        HCLIdent t     -> pretty t
        HCLObject ks h -> vsep $ [(hsep $ prettyKey <$> ks) <+> "{"] <> prettyFields (toList h) <> ["}"]
        HCLList vs     -> "[" <> (hsep $ punctuate comma (pretty <$> vs)) <> "]"

instance Pretty HCLStringPart where
    pretty s = case s of
        HCLStringPlain t  -> pretty t
        HCLStringInterp t -> "${" <> pretty t <> "}"


prettyKey :: Text -> Doc ann
prettyKey t | Text.any (==' ') t  = dquotes $ pretty t
            | otherwise           = pretty t

prettyFields :: [([Text], HCLValue)] -> [Doc ann]
prettyFields = fmap (("  " <>) . pretty . HCLStatementAssignment)


