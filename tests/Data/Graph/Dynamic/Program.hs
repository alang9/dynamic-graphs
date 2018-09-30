{-# LANGUAGE BangPatterns #-}
module Data.Graph.Dynamic.Program
    ( Program
    , Instruction (..)
    , genProgram

    , Interpreter (..)
    , runProgram

    , IntTreeProgram (..)
    , IntGraphProgram (..)
    ) where

import           Control.Monad           (when)
import           Control.Monad.Primitive (PrimMonad (..))
import qualified Data.Graph.Dynamic.Slow as Slow
import           Data.Hashable           (Hashable)
import qualified Data.HashSet            as HS
import qualified Test.QuickCheck         as QC

type Program v = [Instruction v]

data Instruction v
    = InsertVertex v
    | InsertEdge v v Bool
    | DeleteVertex v
    | DeleteEdge v v Bool
    | Connected v v Bool
    deriving (Show)

genProgram
    :: (Eq v, Hashable v)
    => Bool          -- ^ Acyclic only
    -> Int           -- ^ Size of program
    -> Slow.Graph v  -- ^ State of the graph
    -> [v]           -- ^ Pool of Vs to use
    -> QC.Gen (Program v)
genProgram _ size _ _ | size <= 0 = return []
genProgram acyclic size0 graph0 vs0 = do
    let hasVertices = not $ null $ Slow.vertices graph0
    mbInstruction <- QC.frequency $
        [(10, genInsertVertex)] ++
        [(20, genInsertEdge) | hasVertices] ++
        [(1,  genDeleteVertex) | hasVertices] ++
        [(10, genDeleteEdge) | hasVertices] ++
        [(10, genConnected) | hasVertices]

    case mbInstruction of
        Nothing -> genProgram acyclic size0 graph0 vs0
        Just (instr, graph1, vs1) -> (instr :) <$>
            genProgram acyclic (size0 - 1) graph1 vs1
  where
    genInsertVertex =
        let (v, vs1) = case vs0 of
                []       -> error "Ran out of Vs..."
                (x : xs) -> (x, xs)

            graph1 = Slow.insertVertex v graph0 in

        return $ Just (InsertVertex v, graph1, vs1)

    genInsertEdge = do
        x <- QC.elements $ Slow.vertices graph0
        y <- QC.elements $ Slow.vertices graph0
        if Slow.connected x y graph0 && acyclic then
            return Nothing
        else
            let (res, graph1) = Slow.insertEdge x y graph0 in
            return $ Just (InsertEdge x y res, graph1, vs0)

    genDeleteVertex = do
        v <- QC.elements $ Slow.vertices graph0
        let graph1 = Slow.deleteVertex v graph0
        return $ Just (DeleteVertex v, graph1, v : vs0)

    genDeleteEdge = do
        x <- QC.elements $ Slow.vertices graph0
        let nbs = HS.toList $ Slow.neighbours x graph0
        if null nbs then
            return Nothing
        else do
            y <- QC.elements nbs
            let (res, graph1) = Slow.deleteEdge x y graph0
            return $ Just (DeleteEdge x y res, graph1, vs0)

    genConnected = do
        x <- QC.elements $ Slow.vertices graph0
        y <- QC.elements $ Slow.vertices graph0
        let res = Slow.connected x y graph0
        return $ Just (Connected x y res, graph0, vs0)

-- | A graph that we can interpret the program against.
class Interpreter f where
    insertVertex
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> m ()
    insertEdge
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> v -> m Bool
    deleteVertex
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> m ()
    deleteEdge
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> v -> m Bool
    connected
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> v -> m Bool

runProgram
    :: (Eq v, Hashable v, Show v, PrimMonad m, Interpreter f)
    => f (PrimState m) v -> Program v -> m ()
runProgram f = go (0 :: Int)
  where
    go _i [] = return ()
    go !i (instr : instrs) = do

        let expected ==? actual = when (expected /= actual) $ fail $
                "Error after " ++ show i ++
                " instructions, expected " ++ show expected ++
                " but got " ++ show actual ++ " in instruction " ++
                show instr

        case instr of
            InsertVertex x -> insertVertex f x
            InsertEdge x y expected -> do
                actual <- insertEdge f x y
                expected ==? actual
            DeleteVertex x -> deleteVertex f x
            DeleteEdge x y expected -> do
                actual <- deleteEdge f x y
                expected ==? actual
            Connected x y expected -> do
                actual <- connected f x y
                expected ==? actual

        go (i + 1) instrs

newtype IntTreeProgram = IntTreeProgram {unIntTreeProgram :: Program Int}
    deriving (Show)

instance QC.Arbitrary IntTreeProgram where
    arbitrary = QC.sized $ \size -> fmap IntTreeProgram $
        genProgram True size Slow.empty [1 ..]

newtype IntGraphProgram = IntGraphProgram {unIntGraphProgram :: Program Int}
    deriving (Show)

instance QC.Arbitrary IntGraphProgram where
    arbitrary = QC.sized $ \size -> fmap IntGraphProgram $
        genProgram False size Slow.empty [1 ..]
