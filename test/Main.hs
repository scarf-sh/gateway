-- Module      : Main
-- Copyright   : (c) 2020 Scarf Systems, Inc
-- License     : AllRightsReserved
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Scarf.Gateway.Golden qualified
import Scarf.Gateway.ImagePatternTest qualified
import Scarf.Gateway.Test qualified
import Test.Tasty (defaultMain, testGroup)

-- | Main entrypoint for tests
main :: IO ()
main =
  defaultMain . testGroup "Scarf Gateway Specs"
    =<< Scarf.Gateway.Golden.test_gateway_golden
      <> Scarf.Gateway.Test.testTree
      <> Scarf.Gateway.ImagePatternTest.testTree
