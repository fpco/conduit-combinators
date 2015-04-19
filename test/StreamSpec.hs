{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module StreamSpec where

import           Control.Applicative
import qualified Control.Monad
import           Control.Monad (liftM)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State (StateT(..), get, put)
import           Data.Conduit
import           Data.Conduit.Combinators
import           Data.Conduit.Combinators.Internal
import           Data.Conduit.Combinators.Stream
import           Data.Conduit.Internal.Fusion
import           Data.Conduit.Internal.List.Stream (takeS, sourceListS, mapS)
import           Data.Conduit.List (consume, isolate, sourceList)
import qualified Data.List
import           Data.MonoTraversable
import           Data.Monoid (Monoid(..))
import qualified Data.NonNull as NonNull
import           Data.Sequence (Seq)
import qualified Data.Sequences as Seq
import qualified Data.Text.Lazy as TL
import           Data.Vector (Vector)
import qualified Prelude
import           Prelude
    ((.), ($), (=<<), return, id, Maybe(..), Monad, Bool(..), Int,
     Eq, Show, String, Functor, fst, snd)
import qualified Safe
import           System.Directory (removeFile)
import qualified System.IO as IO
import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    it "sourceHandleS works" $ do
        let contents = Prelude.concat $ Prelude.replicate 10000 $ "this is some content\n"
            fp = "tmp"
        IO.writeFile fp contents
        (res, ()) <- IO.withBinaryFile "tmp" IO.ReadMode $ \h ->
            evalStream $ sourceHandleS h emptyStream
        (TL.concat res) `shouldBe` TL.pack contents
        removeFile "tmp"
    describe "Comparing list function to" $ do
        qit "yieldMany" $
            \(mono :: Seq Int) ->
                yieldMany mono `checkProducer`
                otoList mono
        qit "yieldManyS" $
            \(mono :: Seq Int) ->
                yieldManyS mono `checkStreamProducer`
                otoList mono
        qit "repeatM" $
            \(getBlind -> (f :: M Int)) ->
                repeatM f `checkInfiniteProducerM`
                repeatML f
        qit "repeatMS" $
            \(getBlind -> (f :: M Int)) ->
                repeatMS f `checkInfiniteStreamProducerM`
                repeatML f
        qit "repeatWhileM" $
            \(getBlind -> (f :: M Int), getBlind -> g) ->
                repeatWhileM f g `checkInfiniteProducerM`
                repeatWhileML f g
        qit "repeatWhileMS" $
            \(getBlind -> (f :: M Int), getBlind -> g) ->
                repeatWhileMS f g `checkInfiniteStreamProducerM`
                repeatWhileML f g
        qit "foldl1" $
            \(getBlind -> f) ->
                foldl1 f `checkConsumer`
                foldl1L f
        qit "foldl1S" $
            \(getBlind -> f) ->
                foldl1S f `checkStreamConsumer`
                foldl1L f
        qit "all" $
            \(getBlind -> f) ->
                all f `checkConsumer`
                Prelude.all f
        qit "allS" $
            \(getBlind -> f) ->
                allS f `checkStreamConsumer`
                Prelude.all f
        qit "any" $
            \(getBlind -> f) ->
                any f `checkConsumer`
                Prelude.any f
        qit "anyS" $
            \(getBlind -> f) ->
                anyS f `checkStreamConsumer`
                Prelude.any f
        qit "last" $
            \() ->
                last `checkConsumer`
                Safe.lastMay
        qit "lastS" $
            \() ->
                lastS `checkStreamConsumer`
                Safe.lastMay
        qit "lastE" $
            \(getBlind -> f) ->
                let g x = Seq.replicate (Prelude.abs (getSmall (f x))) x :: Seq Int
                 in (map g =$= lastE) `checkConsumer`
                    (lastEL . Prelude.map g :: [Int] -> Maybe Int)
        qit "lastES" $
            \(getBlind -> f) ->
                let g x = Seq.replicate (Prelude.abs (getSmall (f x))) x :: Seq Int
                 in (lastES . mapS g) `checkStreamConsumer`
                    (lastEL . Prelude.map g :: [Int] -> Maybe Int)
        qit "find" $
            \(getBlind -> f) ->
                find f `checkConsumer`
                Data.List.find f
        qit "findS" $
            \(getBlind -> f) ->
                findS f `checkStreamConsumer`
                Data.List.find f
        qit "concatMap" $
            \(getBlind -> (f :: Int -> Seq Int)) ->
                concatMap f `checkConduit`
                concatMapL f
        qit "concatMapS" $
            \(getBlind -> (f :: Int -> Seq Int)) ->
                concatMapS f `checkStreamConduit`
                concatMapL f
        qit "concatMapM" $
            \(getBlind -> (f :: Int -> M (Seq Int))) ->
                concatMapM f `checkConduitM`
                concatMapML f
        qit "concatMapMS" $
            \(getBlind -> (f :: Int -> M (Seq Int))) ->
                concatMapMS f `checkStreamConduitM`
                concatMapML f
        qit "concat" $
            \() ->
                concat `checkConduit`
                (concatL :: [Seq Int] -> [Int])
        qit "concatS" $
            \() ->
                concatS `checkStreamConduit`
                (concatL :: [Seq Int] -> [Int])
        qit "scanl" $
            \(getBlind -> (f :: Int -> Int -> Int), initial) ->
                scanl f initial `checkConduit`
                Prelude.scanl f initial
        qit "scanlS" $
            \(getBlind -> (f :: Int -> Int -> Int), initial) ->
                scanlS f initial `checkStreamConduit`
                Prelude.scanl f initial
        qit "scanlM" $
            \(getBlind -> (f :: Int -> Int -> M Int), initial) ->
                scanlM f initial `checkConduitM`
                scanlML f initial
        qit "scanlMS" $
            \(getBlind -> (f :: Int -> Int -> M Int), initial) ->
                scanlMS f initial `checkStreamConduitM`
                scanlML f initial
        qit "intersperse" $
            \(sep :: Int) ->
                intersperse sep `checkConduit`
                Data.List.intersperse sep
        qit "intersperseS" $
            \(sep :: Int) ->
                intersperseS sep `checkStreamConduit`
                Data.List.intersperse sep
        qit "filterM" $
            \(getBlind -> (f :: Int -> M Bool)) ->
                filterM f `checkConduitM`
                Control.Monad.filterM f
        qit "filterMS" $
            \(getBlind -> (f :: Int -> M Bool)) ->
                filterMS f `checkStreamConduitM`
                Control.Monad.filterM f
    describe "comparing normal conduit function to" $ do
        qit "slidingWindowS" $
            \(getSmall -> n) ->
                slidingWindowS n `checkStreamConduit`
                (\xs -> runIdentity $
                    sourceList xs $= preventFusion (slidingWindow n) $$ consume
                    :: [Seq Int])
        qit "splitOnUnboundedES" $
            \(getBlind -> (f :: Int -> Bool)) ->
                splitOnUnboundedES f `checkStreamConduit`
                (\xs -> runIdentity $
                    sourceList xs $= preventFusion (splitOnUnboundedE f) $$ consume
                    :: [Seq Int])
        qit "initReplicateS" $
            \(getBlind -> (mseed :: M Int), getBlind -> (f :: Int -> M Int), getSmall -> cnt) ->
                initReplicateS mseed f cnt `checkStreamProducerM`
                (preventFusion (initReplicate mseed f cnt) $$ consume)
        qit "initRepeatS" $
            \(getBlind -> (mseed :: M Int), getBlind -> (f :: Int -> M Int)) ->
                initRepeatS mseed f `checkInfiniteStreamProducerM`
                (preventFusion (initRepeat mseed f) $= take 10 $$ consume)
        qit "sinkVectorS" $
            \() -> checkStreamConsumerM'
                unsafePerformIO
                (sinkVectorS :: forall o. StreamConduitM Int o IO.IO (Vector Int))
                (\xs -> sourceList xs $$ preventFusion sinkVector)
        qit "sinkVectorNS" $
            \(getSmall . getNonNegative -> n) -> checkStreamConsumerM'
                unsafePerformIO
                (sinkVectorNS n :: forall o. StreamConduitM Int o IO.IO (Vector Int))
                (\xs -> sourceList xs $$ preventFusion (sinkVectorN n))

instance Arbitrary a => Arbitrary (Seq a) where
    arbitrary = Seq.fromList <$> arbitrary

repeatML :: Monad m => m a -> m [a]
repeatML = Prelude.sequence . Prelude.repeat

repeatWhileML :: Monad m => m a -> (a -> Bool) -> m [a]
repeatWhileML m f = go
  where
    go = do
        x <- m
        if f x
           then liftM (x:) go
           else return []

foldl1L :: (a -> a -> a) -> [a] -> Maybe a
foldl1L _ [] = Nothing
foldl1L f xs = Just $ Prelude.foldl1 f xs

lastEL :: Seq.IsSequence seq
       => [seq] -> Maybe (Element seq)
lastEL = Prelude.foldl go Nothing
  where
    go _ (NonNull.fromNullable -> Just l) = Just (NonNull.last l)
    go mlast _ = mlast

concatMapL :: MonoFoldable mono
           => (a -> mono) -> [a] -> [Element mono]
concatMapL f = Prelude.concatMap (otoList . f)

concatMapML :: (Monad m, MonoFoldable mono)
             => (a -> m mono) -> [a] -> m [Element mono]
concatMapML f = liftM (Prelude.concatMap otoList) . Prelude.mapM f

concatL :: MonoFoldable mono
        => [mono] -> [Element mono]
concatL = Prelude.concatMap otoList

scanlML :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
scanlML f = go
  where
    go l [] = return [l]
    go l (r:rs) = do
        l' <- f l r
        liftM (l:) (go l' rs)

--FIXME: the following code is directly copied from the conduit test
--suite.  How to share this code??

qit :: (Arbitrary a, Testable prop, Show a)
     => String -> (a -> prop) -> Spec
qit n f = it n $ property $ forAll arbitrary f

--------------------------------------------------------------------------------
-- Quickcheck utilities for pure conduits / streams

checkProducer :: (Show a, Eq a) => Source Identity a -> [a] -> Property
checkProducer c l  = checkProducerM' runIdentity c (return l)

checkStreamProducer :: (Show a, Eq a) => StreamSource Identity a -> [a] -> Property
checkStreamProducer s l = checkStreamProducerM' runIdentity s (return l)

checkInfiniteProducer :: (Show a, Eq a) => Source Identity a -> [a] -> Property
checkInfiniteProducer c l = checkInfiniteProducerM' runIdentity c (return l)

checkInfiniteStreamProducer :: (Show a, Eq a) => StreamSource Identity a -> [a] -> Property
checkInfiniteStreamProducer s l = checkInfiniteStreamProducerM' runIdentity s (return l)

checkConsumer :: (Show b, Eq b) => Consumer Int Identity b -> ([Int] -> b) -> Property
checkConsumer c l = checkConsumerM' runIdentity c (return . l)

checkStreamConsumer :: (Show b, Eq b) => StreamConsumer Int Identity b -> ([Int] -> b) -> Property
checkStreamConsumer c l = checkStreamConsumerM' runIdentity c (return . l)

checkConduit :: (Show a, Arbitrary a, Show b, Eq b) => Conduit a Identity b -> ([a] -> [b]) -> Property
checkConduit c l = checkConduitM' runIdentity c (return . l)

checkStreamConduit :: (Show a, Arbitrary a, Show b, Eq b) => StreamConduit a Identity b -> ([a] -> [b]) -> Property
checkStreamConduit c l = checkStreamConduitM' runIdentity c (return . l)

-- checkConduitResult :: (Show a, Arbitrary a, Show b, Eq b, Show r, Eq r) => ConduitM a b Identity r -> ([a] -> ([b], r)) -> Property
-- checkConduitResult c l = checkConduitResultM' runIdentity c (return . l)

checkStreamConduitResult :: (Show a, Arbitrary a, Show b, Eq b, Show r, Eq r) => StreamConduitM a b Identity r -> ([a] -> ([b], r)) -> Property
checkStreamConduitResult c l = checkStreamConduitResultM' runIdentity c (return . l)

--------------------------------------------------------------------------------
-- Quickcheck utilities for conduits / streams in the M monad.

checkProducerM :: (Show a, Eq a) => Source M a -> M [a] -> Property
checkProducerM = checkProducerM' runM

checkStreamProducerM :: (Show a, Eq a) => StreamSource M a -> M [a] -> Property
checkStreamProducerM = checkStreamProducerM' runM

checkInfiniteProducerM :: (Show a, Eq a) => Source M a -> M [a] -> Property
checkInfiniteProducerM = checkInfiniteProducerM' (fst . runM)

checkInfiniteStreamProducerM :: (Show a, Eq a) => StreamSource M a -> M [a] -> Property
checkInfiniteStreamProducerM = checkInfiniteStreamProducerM' (fst . runM)

checkConsumerM :: (Show b, Eq b) => Consumer Int M b -> ([Int] -> M b) -> Property
checkConsumerM  = checkConsumerM' runM

checkStreamConsumerM :: (Show b, Eq b) => StreamConsumer Int M b -> ([Int] -> M b) -> Property
checkStreamConsumerM  = checkStreamConsumerM' runM

checkConduitM :: (Show a, Arbitrary a, Show b, Eq b) => Conduit a M b -> ([a] -> M [b]) -> Property
checkConduitM = checkConduitM' runM

checkStreamConduitM :: (Show a, Arbitrary a, Show b, Eq b) => StreamConduit a M b -> ([a] -> M [b]) -> Property
checkStreamConduitM = checkStreamConduitM' runM

-- checkConduitResultM :: (Show a, Arbitrary a, Show b, Eq b, Show r, Eq r) => ConduitM a b M r -> ([a] -> M ([b], r)) -> Property
-- checkConduitResultM = checkConduitResultM' runM

checkStreamConduitResultM :: (Show a, Arbitrary a, Show b, Eq b, Show r, Eq r) => StreamConduitM a b M r -> ([a] -> M ([b], r)) -> Property
checkStreamConduitResultM = checkStreamConduitResultM' runM

--------------------------------------------------------------------------------
-- Quickcheck utilities for monadic streams / conduits
-- These are polymorphic in which Monad is used.

checkProducerM' :: (Show a, Monad m, Show b, Eq b)
                => (m [a] -> b)
                -> Source m a
                -> m [a]
                -> Property
checkProducerM' f c l =
    f (preventFusion c $$ consume)
    ===
    f l

checkStreamProducerM' :: (Show a, Monad m, Show b, Eq b)
                      => (m [a] -> b)
                      -> StreamSource m a
                      -> m [a]
                      -> Property
checkStreamProducerM' f s l =
    f (liftM fst $ evalStream $ s emptyStream)
    ===
    f l

checkInfiniteProducerM' :: (Show a, Monad m, Show b, Eq b)
                        => (m [a] -> b)
                        -> Source m a
                        -> m [a]
                        -> Property
checkInfiniteProducerM' f s l =
    checkProducerM' f
        (preventFusion s $= isolate 10)
        (liftM (Prelude.take 10) l)

checkInfiniteStreamProducerM' :: (Show a, Monad m, Show b, Eq b)
                              => (m [a] -> b)
                              -> StreamSource m a
                              -> m [a]
                              -> Property
checkInfiniteStreamProducerM' f s l =
    f (liftM snd $ evalStream $ takeS 10 $ s emptyStream)
    ===
    f (liftM (Prelude.take 10) l)

checkConsumerM' :: (Show a, Monad m, Show b, Eq b)
                => (m a -> b)
                -> Consumer Int m a
                -> ([Int] -> m a)
                -> Property
checkConsumerM' f c l = forAll arbitrary $ \xs ->
    f (sourceList xs $$ preventFusion c)
    ===
    f (l xs)

checkStreamConsumerM' :: (Show a, Monad m, Show b, Eq b)
                      => (m a -> b)
                      -> StreamConsumer Int m a
                      -> ([Int] -> m a)
                      -> Property
checkStreamConsumerM' f s l = forAll (arbitrary) $ \xs ->
    f (liftM snd $ evalStream $ s $ sourceListS xs emptyStream)
    ===
    f (l xs)

checkConduitM' :: (Show a, Arbitrary a, Monad m, Show c, Eq c)
               => (m [b] -> c)
               -> Conduit a m b
               -> ([a] -> m [b])
               -> Property
checkConduitM' f c l = forAll arbitrary $ \xs ->
    f (sourceList xs $= preventFusion c $$ consume)
    ===
    f (l xs)

checkStreamConduitM' :: (Show a, Arbitrary a, Monad m, Show c, Eq c)
                     =>  (m [b] -> c)
                     -> StreamConduit a m b
                     -> ([a] -> m [b])
                     -> Property
checkStreamConduitM' f s l = forAll arbitrary $ \xs ->
    f (liftM fst $ evalStream $ s $ sourceListS xs emptyStream)
    ===
    f (l xs)

-- TODO: Fixing this would allow comparing conduit consumers against
-- their list versions.
--
-- checkConduitResultM' :: (Show a, Arbitrary a, Monad m, Show c, Eq c)
--                      => (m ([b], r) -> c)
--                      -> ConduitM a b m r
--                      -> ([a] -> m ([b], r))
--                      -> Property
-- checkConduitResultM' f c l = FIXME forAll arbitrary $ \xs ->
--     f (sourceList xs $= preventFusion c $$ consume)
--     ===
--     f (l xs)

checkStreamConduitResultM' :: (Show a, Arbitrary a, Monad m, Show c, Eq c)
                           =>  (m ([b], r) -> c)
                           -> StreamConduitM a b m r
                           -> ([a] -> m ([b], r))
                           -> Property
checkStreamConduitResultM' f s l = forAll arbitrary $ \xs ->
    f (evalStream $ s $ sourceListS xs emptyStream)
    ===
    f (l xs)

emptyStream :: Monad m => Stream m () ()
emptyStream = Stream (\_ -> return $ Stop ()) (return ())

evalStream :: Monad m => Stream m o r -> m ([o], r)
evalStream (Stream step s0) = go =<< s0
  where
    go s = do
        res <- step s
        case res of
            Stop r -> return ([], r)
            Skip s' -> go s'
            Emit s' x -> liftM (\(l, r) -> (x:l, r)) (go s')

--------------------------------------------------------------------------------
-- Misc utilities

-- Prefer this to creating an orphan instance for Data.Monoid.Sum:

newtype Sum a = Sum a
  deriving (Eq, Show, Arbitrary)

instance Prelude.Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum $ x Prelude.+ y

preventFusion :: a -> a
preventFusion = id
{-# INLINE [0] preventFusion #-}

newtype M a = M (StateT Int Identity a)
  deriving (Functor, Applicative, Monad)

instance Arbitrary a => Arbitrary (M a) where
    arbitrary = do
        f <- arbitrary
        return $ do
            s <- M get
            let (x, s') = f s
            M (put s')
            return x

runM :: M a -> (a, Int)
runM (M m) = runIdentity $ runStateT m 0

--------------------------------------------------------------------------------
-- Utilities from QuickCheck-2.7 (absent in earlier versions)

#if !MIN_VERSION_QuickCheck(2,7,0)
getBlind :: Blind a -> a
getBlind (Blind x) = x

-- | @Small x@: generates values of @x@ drawn from a small range.
-- The opposite of 'Large'.
newtype Small a = Small {getSmall :: a}
    deriving (Prelude.Ord, Prelude.Eq, Prelude.Enum, Prelude.Show, Prelude.Num)

instance Prelude.Integral a => Arbitrary (Small a) where
    arbitrary = Prelude.fmap Small arbitrarySizedIntegral
    shrink (Small x) = Prelude.map Small (shrinkIntegral x)

(===) :: (Show a, Eq a) => a -> a -> Property
x === y = whenFail
    (Prelude.fail $ Prelude.show x Prelude.++ " should match " Prelude.++ Prelude.show y)
    (x Prelude.== y)
#endif
