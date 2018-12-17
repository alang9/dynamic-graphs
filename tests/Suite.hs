import qualified Data.Graph.Dynamic.EulerTour.Tests
import qualified Data.Graph.Dynamic.Internal.Avl.Tests
import qualified Data.Graph.Dynamic.Internal.Random.Tests
import qualified Data.Graph.Dynamic.Internal.Splay.Tests
import qualified Data.Graph.Dynamic.Levels.Tests
import qualified Data.Graph.Dynamic.Thorup2000.Tests
import qualified Data.Graph.Dynamic.Thorup2000.Tests2
import           Test.Framework

main :: IO ()
main = defaultMain
    [ Data.Graph.Dynamic.EulerTour.Tests.tests
    , Data.Graph.Dynamic.Internal.Avl.Tests.tests
    , Data.Graph.Dynamic.Internal.Random.Tests.tests
    , Data.Graph.Dynamic.Internal.Splay.Tests.tests
    , Data.Graph.Dynamic.Levels.Tests.tests
    , Data.Graph.Dynamic.Thorup2000.Tests.tests
    , Data.Graph.Dynamic.Thorup2000.Tests2.tests
    ]
