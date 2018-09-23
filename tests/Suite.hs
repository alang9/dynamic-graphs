import qualified Data.MTree.Splay.Tests
import qualified Properties
import           Test.Framework

main :: IO ()
main = defaultMain
    [ Properties.tests
    , Data.MTree.Splay.Tests.tests
    ]
