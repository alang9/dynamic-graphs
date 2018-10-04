import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Graph.Dynamic.Program as Program
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure)
import qualified System.IO                  as IO
import qualified Test.QuickCheck            as QC
import           Text.Read                  (readMaybe)

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    case args of
        [sizeStr] | Just size <- readMaybe sizeStr -> do
            Program.IntGraphProgram sample <- head <$>
                QC.sample' (QC.resize size QC.arbitrary)
            BL8.putStrLn $ A.encode sample
        _ -> do
            IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " size"
            exitFailure
