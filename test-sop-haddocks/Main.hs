module Main (main) where

import Test.Tasty

import Test.Sanity.TH qualified
import Test.Demo.Dhall qualified

main :: IO ()
main = defaultMain $ testGroup "sop-haddocks" [
      Test.Sanity.TH.tests
    , Test.Demo.Dhall.tests
    ]
