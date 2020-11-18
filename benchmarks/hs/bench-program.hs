import qualified Criterion.Main             as Crit
import qualified Data.Graph.Dynamic.Levels  as Levels
import qualified Data.Graph.Dynamic.Program as Program
import qualified Data.Text.Lazy.IO          as TL

main :: IO ()
main = do
    errOrProgram <- Program.decodeProgram Program.decodeInt <$> TL.getContents

    Crit.defaultMain
        [ Crit.env (either error return errOrProgram) $ \program -> Crit.bench "levels" $ Crit.nfIO $ do
            levels <- Levels.empty'
            Program.runProgram levels (program :: Program.Program Int)
        ]
