{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Action where

import Test.QuickCheck

data ActionType = LinkCut | Toggl

data Action (t :: ActionType) v where
  Cut :: !v -> !v -> Action 'LinkCut v
  Link :: !v -> !v -> Action 'LinkCut v
  Toggle :: !v -> !v -> Action 'Toggl v
  Query :: !v -> !v -> Action a v

deriving instance Show v => Show (Action t v)

deriving instance Functor (Action t)

instance Arbitrary v => Arbitrary (Action 'LinkCut v) where
  arbitrary = oneof
    [ Cut <$> arbitrary <*> arbitrary
    , Link <$> arbitrary <*> arbitrary
    , Query <$> arbitrary <*> arbitrary
    ]
  shrink (Link a b) = Link <$> shrink a <*> shrink b
  shrink (Cut a b) = Cut <$> shrink a <*> shrink b
  shrink (Query a b) = Query <$> shrink a <*> shrink b

instance Arbitrary v => Arbitrary (Action 'Toggl v) where
  arbitrary = oneof
    [ Toggle <$> arbitrary <*> arbitrary
    , Query <$> arbitrary <*> arbitrary
    ]
  shrink (Toggle a b) = Toggle <$> shrink a <*> shrink b
  shrink (Query a b) = Query <$> shrink a <*> shrink b
