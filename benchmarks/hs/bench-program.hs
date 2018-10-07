import qualified Criterion.Main             as Crit
import qualified Data.Graph.Dynamic.Levels  as Levels
import qualified Data.Graph.Dynamic.Program as Program
import qualified Data.Text.Lazy.IO          as TL

main :: IO ()
main = do
    errOrProgram <- Program.decodeProgram Program.decodeInt <$> TL.getContents
    program <- either fail return errOrProgram

    Crit.defaultMain
        [ Crit.bench "levels" $ Crit.nfIO $ do
            levels <- Levels.new
            Program.runProgram levels (program :: Program.Program Int)
        ]
