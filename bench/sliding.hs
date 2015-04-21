{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import Conduit
import Data.Conduit.Combinators (slidingWindow, slidingVectorWindow, unsafeSlidingVectorWindow)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence
import Data.MonoTraversable (Element, unsafeHead)
import qualified Data.Sequences              as Seq
import System.Random (randomRIO)

nn, window :: Int
nn = 20000
window = 100

input :: Source IO Int
input = lift (randomRIO (0, nn)) >>= go nn
  where
    go !k !x | k < 0     = return ()
             | otherwise = yield x >> go (k-1) (x+1)

benchHelper :: (Seq.IsSequence seq, Element seq ~ Int)
            => String
            -> (Int -> Conduit Int IO seq)
            -> String
            -> seq -- ^ dummy
            -> Benchmark
benchHelper name1 conduit name2 _dummy =
      bench (concat [name1, ": ", name2])
    $ whnfIO
    $ input
   $$ conduit window
   =$ sinkForce

sinkForce :: (Seq.IsSequence seq, Element seq ~ Int)
             => Consumer seq IO ()
sinkForce = go 0
  where
    go !x = do
      ms <- await
      case ms of
        Nothing -> return ()
        Just s -> go (x + unsafeHead s)

{-
benchV :: (Element (seq Int) ~ Int)
       => String
       -> seq Int -- ^ dummy
       -> Benchmark
-}
benchV = benchHelper "slidingVectorWindow" slidingVectorWindow
benchUV = benchHelper "unsafeSlidingVectorWindow" unsafeSlidingVectorWindow

{-
benchW :: (Element (seq Int) ~ Int)
       => String
       -> seq Int -- ^ dummy
       -> Benchmark
-}
benchW = benchHelper "slidingWindow" slidingWindow

main :: IO ()
main = defaultMain
    [ benchV "unboxed" VU.empty
    , benchUV "unboxed" VU.empty
    , benchW "unboxed vector" VU.empty
    , benchV "boxed" V.empty
    , benchUV "boxed" V.empty
    , benchW "boxed vector" V.empty
    , benchV "storable" VS.empty
    , benchUV "storable" VS.empty
    , benchW "storable vector" VS.empty
    , benchW "list" []
    , benchW "Seq" Data.Sequence.empty
    ]
