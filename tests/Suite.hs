import qualified Data.MGraph.Tests
import qualified Data.MTree.EulerTour.Tests
import qualified Data.MTree.Splay.Tests
import qualified Data.MTree.FastAvl.Tests
import           Test.Framework

main :: IO ()
main = defaultMain
    [ Data.MGraph.Tests.tests
    , Data.MTree.EulerTour.Tests.tests
    , Data.MTree.Splay.Tests.tests
    , Data.MTree.FastAvl.Tests.tests
    ]
