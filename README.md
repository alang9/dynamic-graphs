# dynamic-graphs

## Summary

A Haskell library for dealing with the _dynamic connectivity_ problem.  Consider
an undirected graph, where edges may be added and removed.  This library allows
you to answer the question "are the nodes X and Y connected" at any point in
time.

This blogpost has some more information about this library:
<https://jaspervdj.be/posts/2018-12-16-dynamic-graphs.html>.

## Installation

`dynamic-graphs` is available on
[hackage](https://hackage.haskell.org/package/dynamic-graphs).  You can install
it using Stack, Cabal, Nix, or whichever tool you prefer.

## Example

```haskell
import qualified Data.Graph.Dynamic.Levels as GD
import qualified Data.Tree as T

main :: IO ()
main = do
    graph <- GD.empty'
    mapM_ (GD.insert_ graph)
        ["Akanu", "Kanoa", "Kekoa", "Kaiwi", "Onakea"]
    GD.link_ graph "Akanu" "Kanoa"
    GD.link_ graph "Akanu" "Kaiwi"
    GD.link_ graph "Akanu" "Onakea"
    GD.link_ graph "Kaiwi" "Onakea"
    GD.link_ graph "Onakea" "Kanoa"
    GD.link_ graph "Kanoa" "Kekoa"

    GD.connected graph "Kaiwi" "Kekoa" >>= print
    GD.cut_ graph "Kaiwi" "Akanu"
    GD.cut_ graph "Onakea" "Akanu"
    GD.cut_ graph "Onakea" "Kanoa"
    GD.connected graph "Kaiwi" "Kekoa" >>= print
    GD.link_ graph "Akanu" "Kaiwi"
    GD.connected graph "Kaiwi" "Kekoa" >>= print
```
