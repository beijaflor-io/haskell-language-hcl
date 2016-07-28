{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module: Data.HCL.PrettyPrint
Description: Exports a @.hcl@ pretty-printer
Copyright: (c) Copyright Pedro Tacla Yamada 2016
License: MIT
Maintainer: tacla.yamada@gmail.com
Stability: experimental
Portability: unknown

This modules contains the 'hcl' pretty-printer for "Data.HCL" data. Declares a
'Pretty' instance for 'HCLStatement' and 'HCLValue'.
-}
module Data.HCL.PrettyPrint
    (
      pPrintHCL
    , Pretty (..)
    , Doc (..)
    )
  where

import           Data.HashMap.Strict            (toList)
import           Data.HCL.Types
import qualified Data.Text                      as Text
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

-- | Pretty-prints a 'HCLDoc' to a 'Doc'
--
-- 'pPrint' restricted to 'HCLDoc'.
--
-- @
-- print (pPrintConf c)
-- @
--
-- See "Text.PrettyPrint"
pPrintHCL :: HCLDoc -> Doc
pPrintHCL = pPrint

instance Pretty HCLStatement where
    pPrint s = case s of
        HCLStatementObject o -> pPrint o
        HCLStatementAssignment (is, v) ->
            hcat (punctuate "." (map (text . Text.unpack) is)) <+>
            "=" <+> pPrint v
    pPrintList _ ss = foldl ($+$) empty (map pPrint ss)

instance Pretty HCLValue where
    pPrint v = case v of
        HCLNumber n -> text (show n)
        HCLString ps -> hcat (map pPrint ps)
        HCLIdent t -> pPrint (Text.unpack t)
        HCLObject ks h -> hsep (map pPrintKey ks) <+> "{" $+$
                          nest 2 (pPrintFields h)
                          $+$ "}"
        HCLList vs -> hcat (punctuate comma (map pPrint vs))

instance Pretty HCLStringPart where
    pPrint s = case s of
        HCLStringPlain t -> pPrint (Text.unpack t)
        HCLStringInterp t -> "#{" <> pPrint (Text.unpack t) <> "}"

pPrintKey k = let s = Text.unpack k
              in if ' ' `elem` s then text (show s)
                 else text s

pPrintFields h = pPrintList undefined
    (map (\(f, v) -> HCLStatementAssignment (f, v)) (toList h))

