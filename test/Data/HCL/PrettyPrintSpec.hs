{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HCL.PrettyPrintSpec where

import           Data.HCL.PrettyPrint
import           Data.HCL.Types
import           Test.Hspec
import           Text.PrettyPrint.HughesPJClass

spec :: Spec
spec =
    describe "pPrint" $
        it "pretty prints an HCL document" $ do
            let input :: HCLDoc
                input =  [ HCLStatementAssignment ( ["hello", "world"]
                                                  , HCLString [HCLStringPlain "Hello World"]
                                                  )
                         , HCLStatementAssignment (["var"], HCLNumber 10)
                         , HCLStatementObject
                           (HCLObject
                             ["resource", "something h"]
                             [ (["something"], HCLString [HCLStringPlain "Here"])
                             ]
                           )
                         ]
            prettyShow input `shouldBe`
                -- We use init because there's no trailing newline
                init (unlines [ "hello.world = \"Hello World\""
                              , "var = 10.0"
                              , "resource \"something h\" {"
                              , "  something = \"Here\""
                              , "}"
                              ])
