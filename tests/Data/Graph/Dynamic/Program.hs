{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Graph.Dynamic.Program
    ( Program
    , Instruction (..)
    , genProgram

    , Interpreter (..)
    , runProgram

    , IntTreeProgram (..)
    , IntGraphProgram (..)
    ) where

import           Control.Monad                (void, when)
import           Control.Monad.Primitive      (PrimMonad (..))
import qualified Data.Aeson                   as A
import qualified Data.Graph.Dynamic.EulerTour as ET
import qualified Data.Graph.Dynamic.Levels    as Levels
import qualified Data.Graph.Dynamic.Slow      as Slow
import           Data.Hashable                (Hashable)
import qualified Data.HashSet                 as HS
import           Data.List                    ((\\))
import           Data.Maybe                   (fromMaybe)
import qualified Test.QuickCheck              as QC

type Program v = [Instruction v]

data Instruction v
    = InsertVertex v
    | InsertEdge v v
    | DeleteVertex v
    | DeleteEdge v v
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
    let hasSomeVertices = case Slow.vertices graph0 of
            (_ : _ : _) -> True
            _           -> False

    mbInstruction <- QC.frequency $
        [(10, genInsertVertex)] ++
        [(30, genInsertEdge) | hasSomeVertices] ++
        [(1,  genDeleteVertex) | hasSomeVertices] ++
        [(10, genDeleteEdge) | hasSomeVertices] ++
        [(30, genConnected) | hasSomeVertices]

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
        y <- QC.elements $ Slow.vertices graph0 \\ [x]
        if  | Slow.connected x y graph0 && acyclic ->
                return Nothing
            | Slow.hasEdge x y graph0 ->
                return Nothing
            | otherwise ->
                let (_, graph1) = Slow.insertEdge x y graph0 in
                return $ Just (InsertEdge x y, graph1, vs0)

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
            let (_, graph1) = Slow.deleteEdge x y graph0
            return $ Just (DeleteEdge x y, graph1, vs0)

    genConnected = do
        x <- QC.elements $ Slow.vertices graph0
        y <- QC.elements $ Slow.vertices graph0 \\ [x]
        let res = Slow.connected x y graph0
        return $ Just (Connected x y res, graph0, vs0)

-- | A graph that we can interpret the program against.
class Interpreter f where
    insertVertex
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> m ()
    insertEdge
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> v -> m ()
    deleteVertex
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> m ()
    deleteEdge
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> v -> m ()
    connected
        :: (Eq v, Hashable v, PrimMonad m)
        => f (PrimState m) v -> v -> v -> m Bool

instance Interpreter Levels.Graph where
    insertVertex    = Levels.insertVertex
    insertEdge      = Levels.insertEdge
    deleteVertex    = Levels.deleteVertex
    deleteEdge      = Levels.deleteEdge
    connected f x y = fromMaybe False <$> Levels.connected f x y

instance Interpreter ET.Forest where
    insertVertex     = ET.insertVertex
    insertEdge f x y = void $ ET.insertEdge f x y
    deleteVertex     = ET.deleteVertex
    deleteEdge f x y = void $ ET.deleteEdge f x y
    connected f x y  = fromMaybe False <$> ET.connected f x y

runProgram
    :: (Eq v, Hashable v, Show v, PrimMonad m, Interpreter f)
    => f (PrimState m) v -> Program v -> m ()
runProgram f = go (0 :: Int)
  where
    go _i [] = return ()
    go !i (instr : instrs) = do

        case instr of
            InsertVertex x -> insertVertex f x
            InsertEdge x y -> insertEdge f x y
            DeleteVertex x -> deleteVertex f x
            DeleteEdge x y -> deleteEdge f x y
            Connected x y expected -> do
                actual <- connected f x y
                when (expected /= actual) $ fail $
                    "Error after " ++ show i ++
                    " instructions, expected " ++ show expected ++
                    " but got " ++ show actual ++ " in instruction " ++
                    show instr

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

--------------------------------------------------------------------------------

instance A.ToJSON v => A.ToJSON (Instruction v) where
    toJSON (InsertVertex x) = A.object
        ["tag" A..= tagInsert, "x" A..= x]

    toJSON (InsertEdge x y) = A.object
        ["tag" A..= tagLink, "x" A..= x, "y" A..= y]

    toJSON (DeleteVertex x) = A.object
        ["tag" A..= tagDelete, "x" A..= x]

    toJSON (DeleteEdge x y) = A.object
        ["tag" A..= tagCut, "x" A..= x, "y" A..= y]

    toJSON (Connected x y expect) = A.object
        ["tag" A..= tagConnected, "x" A..= x, "y" A..= y, "expect" A..= expect]

instance A.FromJSON v => A.FromJSON (Instruction v) where
    parseJSON = A.withObject "FromJSON Instruction" $ \o -> do
        tag <- o A..: "tag"
        if  | tag == tagInsert    -> InsertVertex <$> o A..: "x"
            | tag == tagLink      -> InsertEdge <$> o A..: "x" <*> o A..: "y"
            | tag == tagDelete    -> DeleteVertex <$> o A..: "x"
            | tag == tagCut       -> DeleteEdge <$> o A..: "x" <*> o A..: "y"
            | tag == tagConnected -> Connected <$> o A..: "x" <*> o A..: "y" <*> o A..: "expect"
            | otherwise           -> fail $ "Unknown tag: " ++ show tag

tagInsert, tagLink, tagDelete, tagCut, tagConnected :: Int
tagInsert    = 0
tagLink      = 1
tagDelete    = 2
tagCut       = 3
tagConnected = 4
