{-# LANGUAGE DeriveGeneric #-}

module Action where

import GHC.Generics
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

data Action
  = Cut !Int !Int
  | Link !Int !Int
  | Query !Int !Int
  deriving (Show, Generic)

instance Arbitrary Action where
  arbitrary = oneof
    [ Cut <$> arbitrary <*> arbitrary
    , Link <$> arbitrary <*> arbitrary
    , Query <$> arbitrary <*> arbitrary
    ]
  shrink = genericShrink
