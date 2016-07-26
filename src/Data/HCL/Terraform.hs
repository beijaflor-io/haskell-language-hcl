module Data.HCL.Terraform where

import           Data.HCL.Class
import           Data.HCL.Types

newtype Terraform = Terraform [TerraformStatement]

data TerraformStatement =
    TerraformStatement

instance ToHCLDoc Terraform where
    toHCLDoc = undefined
