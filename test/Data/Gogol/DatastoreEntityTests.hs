{-# LANGUAGE TemplateHaskell #-}

module Data.Gogol.DatastoreEntityTests where

import           Test.QuickCheck          hiding (output)
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck    hiding (output)
import           Test.Tasty.TH

case_sanity_prevails =
  1 @?= 0

datastoreEntityTests :: TestTree
datastoreEntityTests = $(testGroupGenerator)

