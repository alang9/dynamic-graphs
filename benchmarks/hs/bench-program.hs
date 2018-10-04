import qualified Criterion.Main             as Crit
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Graph.Dynamic.Levels  as Levels
import qualified Data.Graph.Dynamic.Program as Program

main :: IO ()
main = do
    errOrProgram <- A.eitherDecode' <$> BL.getContents
    program <- either (fail . show) return errOrProgram

    Crit.defaultMain
        [ Crit.bench "levels" $ Crit.nfIO $ do
            levels <- Levels.new
            Program.runProgram levels (program :: Program.Program Int)
        ]
