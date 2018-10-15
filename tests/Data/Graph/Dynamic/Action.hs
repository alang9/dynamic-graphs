-- | Generate arbitrary "actions" (cut, link...) to test the connectivity
-- algorithms.
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Graph.Dynamic.Action
    ( ActionType (..)
    , Action (..)

    , runSlowForestAction
    , runSlowGraphAction
    ) where

import qualified Data.Graph.Dynamic.Slow as Slow
import           Data.Hashable           (Hashable)
import           Test.QuickCheck

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
  shrink (Link a b)  = Link <$> shrink a <*> shrink b
  shrink (Cut a b)   = Cut <$> shrink a <*> shrink b
  shrink (Query a b) = Query <$> shrink a <*> shrink b

instance Arbitrary v => Arbitrary (Action 'Toggl v) where
  arbitrary = oneof
    [ Toggle <$> arbitrary <*> arbitrary
    , Query <$> arbitrary <*> arbitrary
    ]
  shrink (Toggle a b) = Toggle <$> shrink a <*> shrink b
  shrink (Query a b)  = Query <$> shrink a <*> shrink b

runSlowForestAction
    :: (Eq v, Hashable v)
    => Slow.Graph v -> Action t v -> (Slow.Graph v, Maybe Bool)
runSlowForestAction graph (Cut x y) =
    (Slow.cut x y graph, Nothing)
runSlowForestAction graph (Link x y)
    | Slow.connected x y graph = (graph, Nothing)
    | otherwise                = (Slow.link x y graph, Nothing)
runSlowForestAction graph (Toggle x y)
    | Slow.edge x y graph      = (Slow.cut x y graph, Nothing)
    | Slow.connected x y graph = (graph, Nothing)
    | otherwise                = (Slow.link x y graph, Nothing)
runSlowForestAction graph (Query x y) =
    (graph, Just (Slow.connected x y graph))

runSlowGraphAction
    :: (Eq v, Hashable v)
    => Slow.Graph v -> Action t v -> (Slow.Graph v, Maybe Bool)
runSlowGraphAction graph (Cut x y) =
    (Slow.cut x y graph, Nothing)
runSlowGraphAction graph (Link x y) =
    (Slow.link x y graph, Nothing)
runSlowGraphAction graph (Toggle x y)
    | Slow.edge x y graph = (Slow.cut x y graph, Nothing)
    | otherwise           = (Slow.link x y graph, Nothing)
runSlowGraphAction graph (Query x y) =
    (graph, Just (Slow.connected x y graph))
