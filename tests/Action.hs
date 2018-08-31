{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Action where

import GHC.Generics
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

data ActionType = LinkCut | Toggl

data Action (t :: ActionType) where
  Cut :: !Int -> !Int -> Action LinkCut
  Link :: !Int -> !Int -> Action LinkCut
  Toggle :: !Int -> !Int -> Action Toggl
  Query :: !Int -> !Int -> Action a

deriving instance Show (Action t)

instance Arbitrary (Action 'LinkCut) where
  arbitrary = oneof
    [ Cut <$> arbitrary <*> arbitrary
    , Link <$> arbitrary <*> arbitrary
    , Query <$> arbitrary <*> arbitrary
    ]
  shrink (Link a b) = Link <$> shrink a <*> shrink b
  shrink (Cut a b) = Cut <$> shrink a <*> shrink b
  shrink (Query a b) = Query <$> shrink a <*> shrink b

instance Arbitrary (Action 'Toggl) where
  arbitrary = oneof
    [ Toggle <$> arbitrary <*> arbitrary
    , Query <$> arbitrary <*> arbitrary
    ]
  shrink (Toggle a b) = Toggle <$> shrink a <*> shrink b
  shrink (Query a b) = Query <$> shrink a <*> shrink b
