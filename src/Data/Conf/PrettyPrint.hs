{-|
Module: Data.Conf.PrettyPrint
Description: Pretty-printer for 'Data.Conf'
Copyright: (c) Copyright Pedro Tacla Yamada 2016
License: MIT
Maintainer: tacla.yamada@gmail.com
Stability: experimental
Portability: unknown

Pretty-printer for "Data.Conf". Declares a 'Pretty' instance for
'ConfStatement'.
-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Conf.PrettyPrint
    (
      pPrintConf
    , Pretty (..)
    , Doc (..)
    )
  where

import           Data.Conf.Types
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Text.PrettyPrint.HughesPJClass

-- | Pretty-prints a 'Conf' to a 'Doc'
--
-- 'pPrint' restricted to 'Conf'
--
-- @
-- print (pPrintConf c)
-- @
--
-- See "Text.PrettyPrint"
pPrintConf = pPrint :: Conf -> Doc

instance Pretty ConfStatement where
    pPrint s = case s of
        ConfStatementEmptyLine -> text ""
        ConfStatementComment (Comment c) ->
            "#" <> ttext c
        ConfStatementBlock (Block ks ss) ->
            thsep ks <+> "{"
               $+$ nest 2 (pPrintList (PrettyLevel 0) ss) $+$
            "}"
        ConfStatementExpression (Expression t ts) ->
            ttext t <+> thsep ts <> ";"
    pPrintList _ ss = foldl ($+$) empty (map pPrint ss)

thsep :: [Text] -> Doc
thsep = hsep . map ttext

ttext :: Text -> Doc
ttext = text . Text.unpack
