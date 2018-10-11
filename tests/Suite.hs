import qualified Data.Graph.Dynamic.EulerTour.Tests
import qualified Data.Graph.Dynamic.EulerTour.Tests2
import qualified Data.Graph.Dynamic.Internal.BitVector.Tests
import qualified Data.Graph.Dynamic.Internal.FastAvl.Tests
import qualified Data.Graph.Dynamic.Internal.Splay.Tests
import qualified Data.Graph.Dynamic.Levels.Tests
import qualified Data.Graph.Dynamic.Levels.Tests2
import qualified Data.Graph.Dynamic.Thorup2000.Tests
import           Test.Framework

main :: IO ()
main = defaultMain
    [ Data.Graph.Dynamic.EulerTour.Tests.tests
    , Data.Graph.Dynamic.EulerTour.Tests2.tests
    , Data.Graph.Dynamic.Internal.BitVector.Tests.tests
    , Data.Graph.Dynamic.Internal.FastAvl.Tests.tests
    , Data.Graph.Dynamic.Internal.Splay.Tests.tests
    , Data.Graph.Dynamic.Levels.Tests.tests
    , Data.Graph.Dynamic.Levels.Tests2.tests
    , Data.Graph.Dynamic.Thorup2000.Tests.tests
    ]
