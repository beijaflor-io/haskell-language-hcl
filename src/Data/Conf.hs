{-|
Module: Data.Conf
Description: Exports a @.conf@ Megaparsec parser through 'conf'
Copyright: (c) Copyright Pedro Tacla Yamada 2016
License: MIT
Maintainer: tacla.yamada@gmail.com
Stability: experimental
Portability: unknown

This modules contains the 'conf' Megaparsec parser for @.conf@ files and Pretty
instance.
-}
module Data.Conf
    (
      -- * Entry-points
      conf
    , pPrintConf
    , runParser
      -- * Types
    , Conf (..)
    , ConfStatement (..)
    , Block (..)
    , Comment (..)
    , Expression (..)
      -- * Parser
    , confStatement
    , confStatementLines
    , block
    , expression
    , argument
      -- * Pretty-printer
    , Pretty(..)
    , pPrint
    )
  where

import           Data.Conf.Internal
import           Data.Conf.PrettyPrint
import           Data.Conf.Types
import           Text.Megaparsec       (runParser)
