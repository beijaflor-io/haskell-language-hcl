{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HCL.PrettyPrintSpec where

import           Data.HCL.Types
import           Data.Text.Prettyprint.Doc               (defaultLayoutOptions,
                                                          layoutPretty, pretty)
import           Data.Text.Prettyprint.Doc.Render.String (renderString)

import           Test.Hspec

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
                             [  ( ["something"], HCLString [HCLStringPlain "Here"] )
                             ,  ( ["something", "else"], HCLString [HCLStringPlain "There"] )
                             ]
                           )
                         ]
            renderString (layoutPretty defaultLayoutOptions $ pretty input) `shouldBe`
                -- We use init because there's no trailing newline
                init (unlines [ "hello.world = \"Hello World\""
                              , "var = 10.0"
                              , "resource \"something h\" {"
                              , "  something.else = \"There\""
                              , "  something = \"Here\""
                              , "}"
                              ])
