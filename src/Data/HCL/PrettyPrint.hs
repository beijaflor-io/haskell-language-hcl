{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.HCL.PrettyPrint where

import Data.HashMap.Strict (toList)
import           Data.HCL.Types
import qualified Data.Text as Text
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

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
                          (nest 2 (pPrintFields h))
                          $+$ "}"
        HCLList vs -> hcat (punctuate comma (map pPrint vs))

pPrintKey k = let s = Text.unpack k
              in if ' ' `elem` s then text (show s)
                 else text s

pPrintFields h = pPrintList undefined
    (map (\(f, v) -> HCLStatementAssignment (f, v)) (toList h))

instance Pretty HCLStringPart where
    pPrint s = case s of
        HCLStringPlain t -> pPrint (Text.unpack t)
        HCLStringInterp t -> "#{" <> pPrint (Text.unpack t) <> "}"
