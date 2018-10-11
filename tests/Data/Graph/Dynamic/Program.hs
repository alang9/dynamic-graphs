{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Graph.Dynamic.Program
    ( Program
    , Instruction (..)
    , genProgram

    , Interpreter (..)
    , runProgram

    , IntTreeProgram (..)
    , IntGraphProgram (..)

    , encodeProgram
    , decodeProgram
    , encodeInt
    , decodeInt
    ) where

import Control.DeepSeq
import           Control.Monad                    (void, when)
import           Control.Monad.Primitive          (PrimMonad (..))
import qualified Data.Graph.Dynamic.EulerTour     as ET
import           Data.Graph.Dynamic.Internal.Tree (Tree)
import qualified Data.Graph.Dynamic.Levels        as Levels
import qualified Data.Graph.Dynamic.Slow          as Slow
import           Data.Hashable                    (Hashable)
import qualified Data.HashSet                     as HS
import           Data.List                        (intersperse, (\\))
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TLB
import GHC.Generics
import qualified Test.QuickCheck                  as QC
import           Text.Read                        (readMaybe)

type Program v = [Instruction v]

data Instruction v
    = InsertVertex v
    | InsertEdge v v
    | DeleteVertex v
    | DeleteEdge v v
    | Connected v v Bool
    deriving (Show, Generic)

instance (NFData v) => NFData (Instruction v)

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

instance Tree t => Interpreter (Levels.Graph t) where
    insertVertex    = Levels.insertVertex
    insertEdge f x y = void $ Levels.insertEdge f x y
    deleteVertex    = Levels.deleteVertex
    deleteEdge      = Levels.deleteEdge
    connected f x y = fromMaybe False <$> Levels.connected f x y

instance Tree t => Interpreter (ET.Graph t) where
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

encodeProgram
    :: (v -> T.Text) -> Program v -> TL.Text
encodeProgram encodeVertex =
    TLB.toLazyText . mconcat . intersperse "\n" . map encodeInstruction
  where
    x <+> y = x <> " " <> y
    v       = TLB.fromText . encodeVertex
    b False = "false"
    b True  = "true"

    encodeInstruction (InsertVertex x)  = "insert" <+> v x
    encodeInstruction (InsertEdge x y)  = "link" <+> v x <+> v y
    encodeInstruction (DeleteVertex x)  = "delete" <+> v x
    encodeInstruction (DeleteEdge x y)  = "cut" <+> v x <+> v y
    encodeInstruction (Connected x y e) = "connected" <+> v x <+> v y <+> b e

decodeProgram
    :: (T.Text -> Either String v) -> TL.Text -> Either String (Program v)
decodeProgram decodeVertex =
    mapM decodeInstruction . TL.lines
  where
    v         = decodeVertex
    b "false" = return False
    b "true"  = return True
    b x       = Left $ "Can't decode bool: " ++ T.unpack x

    decodeInstruction line = case T.words (TL.toStrict line) of
        ["insert", x]          -> InsertVertex <$> v x
        ["link", x, y]         -> InsertEdge <$> v x <*> v y
        ["delete", x]          -> DeleteVertex <$> v x
        ["cut", x, y]          -> DeleteEdge <$> v x <*> v y
        ["connected", x, y, e] -> Connected <$> v x <*> v y <*> b e
        _                      -> Left $
            "Can't decode instruction: " ++ TL.unpack line

encodeInt :: Int -> T.Text
encodeInt = T.pack . show

decodeInt :: T.Text -> Either String Int
decodeInt t = case readMaybe (T.unpack t) of
    Nothing -> Left $ "Can't decode int: " ++ T.unpack t
    Just x  -> Right x
