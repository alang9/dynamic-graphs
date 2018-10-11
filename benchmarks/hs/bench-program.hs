import           Control.Monad.Primitive            (RealWorld)
import qualified Criterion.Main                     as Crit
import qualified Data.Graph.Dynamic.Internal.Random as Random
import qualified Data.Graph.Dynamic.Levels          as Levels
import qualified Data.Graph.Dynamic.Program         as Program
import qualified Data.Text.Lazy.IO                  as TL

main :: IO ()
main = do
    errOrProgram <- Program.decodeProgram Program.decodeInt <$> TL.getContents

    Crit.defaultMain
        [ Crit.env (either fail return errOrProgram) $ \program -> Crit.bench "levels" $ Crit.nfIO $ do
            levels <- Levels.new :: IO (Levels.Graph Random.Tree RealWorld Int)
            Program.runProgram levels (program :: Program.Program Int)
        ]
