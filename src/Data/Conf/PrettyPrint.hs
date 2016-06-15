{-# LANGUAGE OverloadedStrings #-}
module Data.Conf.PrettyPrint
    ( module Text.PrettyPrint.HughesPJClass
    )
  where

import           Data.Conf
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Text.PrettyPrint.HughesPJClass

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

-- test = pPrint example
