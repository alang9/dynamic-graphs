import qualified Data.Graph.Dynamic.EulerTour.Tests
import qualified Data.Graph.Dynamic.Internal.FastAvl.Tests
import qualified Data.Graph.Dynamic.Internal.Random.Tests
import qualified Data.Graph.Dynamic.Internal.Splay.Tests
import qualified Data.Graph.Dynamic.Levels.Tests
import           Test.Framework

main :: IO ()
main = defaultMain
    [ Data.Graph.Dynamic.EulerTour.Tests.tests
    , Data.Graph.Dynamic.Internal.FastAvl.Tests.tests
    , Data.Graph.Dynamic.Internal.Random.Tests.tests
    , Data.Graph.Dynamic.Internal.Splay.Tests.tests
    , Data.Graph.Dynamic.Levels.Tests.tests
    ]
