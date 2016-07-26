
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HCL.TerraformSpec where

import           Data.HCL.Terraform
import           Data.HCL.Types
import           Test.Hspec

spec :: Spec
spec =
    describe "terraform" $
        describe "ec2" $
            it "works" $ pending
