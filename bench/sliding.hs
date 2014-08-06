{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Criterion.Main
import Conduit
import Data.Conduit.Combinators (slidingWindow, slidingVector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence
import Data.MonoTraversable (Element)

input :: Monad m => Source m Int
input = yieldMany [1..100000]

benchHelper :: String
            -> (Int -> Conduit Int IO a)
            -> String
            -> a -- ^ dummy
            -> Benchmark
benchHelper name1 conduit name2 _dummy =
      bench (concat [name1, ": ", name2])
    $ whnfIO
    $ input
   $$ conduit 30
   =$ sinkNull

{-
benchV :: (Element (seq Int) ~ Int)
       => String
       -> seq Int -- ^ dummy
       -> Benchmark
-}
benchV = benchHelper "slidingVector" slidingVector

{-
benchW :: (Element (seq Int) ~ Int)
       => String
       -> seq Int -- ^ dummy
       -> Benchmark
-}
benchW = benchHelper "slidingWindow" slidingWindow

main :: IO ()
main = defaultMain
    [ benchV "boxed" V.empty
    , benchW "boxed vector" V.empty
    , benchV "storable" VS.empty
    , benchW "storable vector" VS.empty
    , benchV "unboxed" VU.empty
    , benchW "unboxed vector" VU.empty
    , benchW "list" []
    , benchW "Seq" Data.Sequence.empty
    ]
