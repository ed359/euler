{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Problem014 (main, solve) where

import Data.Foldable (maximumBy)
import Data.Ord (comparing)

import Data.Ix (Ix, inRange, range)
import Data.Array (Array)
import qualified Data.Array as A

import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA

import Control.Monad.ST (ST)
import Data.Array.ST (runSTArray, runSTUArray, STArray, STUArray)
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IA
import Data.Array.MArray (MArray)
import qualified Data.Array.MArray as MA
import Control.Monad (forM_, when)

import Data.Functor.Identity (runIdentity)

-- import Debug.Trace (trace, traceShow)

-----------------------------------------------------------------------------------------------------------------------
-- Section 1. Basic stuff
-----------------------------------------------------------------------------------------------------------------------

-- The Collatz function with input n recurses on next n.
next :: Integral a => a -> a
next n = if even n then n `div` 2 else 3*n + 1

-- A naive helper function for testing.
collatzSequence :: Int -> [Int]
collatzSequence 1 = [1]
collatzSequence n = n : collatzSequence (next n)

-- An abstract version of the recursive Collatz length function where the recursive calls are to a parameter f
collatzLengthAbstract :: (Int -> Int) -> Int -> Int
collatzLengthAbstract _ 1 = 1
collatzLengthAbstract f n = 1 + f (next n)

-- The naive recursive solution calls itself as "f".
naiveCollatzLength :: Int -> Int
naiveCollatzLength = collatzLengthAbstract naiveCollatzLength


-----------------------------------------------------------------------------------------------------------------------
-- Section 2. Memoization with fixed immutable arrays
-----------------------------------------------------------------------------------------------------------------------

-- A few big types that can be aliased.
type IArrayConstructorList a i e = (i, i) -> [(i, e)] -> a i e
type IArrayConstructorFunc a i e = (i, i) -> (i -> e) -> a i e

-- Generic function to construct an array arr such that arr ! i = f i.
buildMemo :: (Ix i) => IArrayConstructorList a i e -> IArrayConstructorFunc a i e
buildMemo arrayConstructor memBounds f = arrayConstructor memBounds $ map (\i -> (i, f i)) $ range memBounds

-- Generic function to memoize a range of values in an immutable array, filled with the supplied constructor.
memoizeRangeIA :: (Ix i, IArray a e) => IArrayConstructorFunc a i e -> (i, i) -> ((i -> e) -> i -> e) -> i -> e
memoizeRangeIA arrayBuilder memBounds abstractF = arrayLookup
    where
        memoArray = arrayBuilder memBounds (abstractF arrayLookup)
        arrayLookup j = if inRange memBounds j then memoArray IA.! j else abstractF arrayLookup j

-- Memoized Collatz length function using an immutable non-strict (boxed) array.
collatzLengthMemoA :: Int -> Int -> Int
collatzLengthMemoA maxMemo = memoizeRangeA (buildMemo A.array) (1, maxMemo) collatzLengthAbstract
    where
        -- This is technically superfluous since GHC manages to infer the right types and constraints for the Array type.
        memoizeRangeA :: (IArrayConstructorFunc Array Int Int -> (Int, Int) -> ((Int -> Int) -> Int -> Int) -> Int -> Int)
        memoizeRangeA = memoizeRangeIA

-- Memoized Collatz length function using an immutable array of unboxed Ints, which is thus strict in its elements.
collatzLengthMemoUA :: Int -> Int -> Int
collatzLengthMemoUA maxMemo = memoizeRangeUA (buildMemo UA.array) (1, maxMemo) collatzLengthAbstract
    where
        -- This is necessary since GHC does not manage to infer the right types and constraints for the UArray type.
        -- Perhaps some language extensions allow another way around the type inference/monomorphism restriction beast
        -- that starts rearing its head here
        memoizeRangeUA :: (IArrayConstructorFunc UArray Int Int -> (Int, Int) -> ((Int -> Int) -> Int -> Int) -> Int -> Int)
        memoizeRangeUA = memoizeRangeIA


-----------------------------------------------------------------------------------------------------------------------
-- Section 3. Memoization with a dynamically sized (though bounded in maximum size) immutable array
-----------------------------------------------------------------------------------------------------------------------

-- Generic function to construct a function which takes an existing memoizing array and 
-- gives a new one with larger range, copying in the values of the existing array
type IAExtender a i e = (i, i) -> (i -> e) -> a i e -> a i e
extendMemoIA :: (Ix i, IArray a e) => IAExtender a i e
extendMemoIA memBounds f arr = IA.array memBounds $ IA.assocs arr ++ map (\x -> (x, f x)) (dropWhile (inRange (IA.bounds arr)) (range memBounds))

memoizeDynamicIA :: (Ix i, Num i, IArray a e) => i -> a i e -> ((i -> e) -> i -> e) -> i -> e
memoizeDynamicIA maxMemo arr abstractF x
    | inRange currBounds x = arr IA.! x
    | inRange maxBounds x  = memoizeDynamicIA maxMemo newArr abstractF x
    | otherwise            = abstractF (memoizeDynamicIA maxMemo arr abstractF) x
    where
        currBounds@(a, b) = IA.bounds arr
        maxBounds = (a, maxMemo)
        newBounds = (a, max x (2*b))
        newArr = extendMemoIA newBounds (abstractF (memoizeDynamicIA maxMemo newArr abstractF)) arr

-- Consider adding some debug output to track what's happening
--     | inRange currBounds x = trace ("access arr[" ++ show x ++ "]" ++ " = " ++ show (arr IA.! x)) arr IA.! x
--     | inRange maxBounds x  = trace ("grow to " ++ show newBounds ++ " for " ++ show x) memoizeDynamicIA maxMemo newArr abstractF x
--     | otherwise            = trace ("recur for " ++ show x) abstractF (memoizeDynamicIA maxMemo arr abstractF) x

collatzLengthMemoDA :: Int -> Int -> Int
collatzLengthMemoDA maxMemo = memoizeDynamicIA maxMemo baseCaseArr collatzLengthAbstract
    where
        -- Superfluous type annotation because Array is the easy case
        baseCaseArr :: Array Int Int
        baseCaseArr = buildMemo A.array (1, 1) naiveCollatzLength

collatzLengthMemoDUA :: Int -> Int -> Int
collatzLengthMemoDUA maxMemo = memoizeDynamicIA maxMemo baseCaseArr collatzLengthAbstract
    where
        -- Necessary type annotation because UArray is not an easy case
        baseCaseArr :: UArray Int Int
        baseCaseArr = buildMemo UA.array (1, 1) naiveCollatzLength


-----------------------------------------------------------------------------------------------------------------------
-- Section 4. Using mutability to construct a fixed memoization table in ST
-----------------------------------------------------------------------------------------------------------------------

-- We got specifically for top-down memoization because the Collatz function is not suited to bottom-up computation.
-- Specifically, we do not know which inputs we will later call naiveCollatzLength with when we call 
-- naiveCollatzLength n. Contrast this with the e.g. the Fibonacci numbers where to get fib n we need fib i for i up to
-- n.

-- Early attemps are quite specialized to the Collatz length function: -1 is not a generic "null" value and
-- collatzLengthMutableLookup encodes the specifics of the recursion. Also, we have to memoize up to the desired value 
-- here which is somewhat arbitrary.

-- Construct a non-strict array in ST.
collatzLengthMemoST :: Int -> Int
collatzLengthMemoST n = memoArr IA.! n where
    memBounds = (1, n)
    memoArr = runSTArray $ do
        arr <- MA.newArray memBounds (-1)
        MA.writeArray arr 1 1
        collatzLengthMutableLookup arr n
        return arr

-- Construct an unboxed array in ST.
collatzLengthMemoSTU :: Int -> Int
collatzLengthMemoSTU n = memoArr IA.! n where
    memBounds = (1, n)
    memoArr = runSTUArray $ do
        arr <- MA.newArray memBounds (-1)
        MA.writeArray arr 1 1
        collatzLengthMutableLookup arr n
        return arr

-- The engine of these versions is a recursive lookup function that gracefully recurses if the requested input is out
-- of bounds or hasn't yet been calculated, and when new in-bounds values are computed it writes them to the array.
collatzLengthMutableLookup :: (MArray a e m, Ix i, Integral i, Num e, Eq e) => a i e -> i -> m e
collatzLengthMutableLookup arr j = do
    memBounds <- MA.getBounds arr
    if inRange memBounds j then do
        l <- MA.readArray arr j
        when (l == -1) $ do
            l' <- (+1) <$> collatzLengthMutableLookup arr j'
            MA.writeArray arr j l'
        MA.readArray arr j
    else (+1) <$> collatzLengthMutableLookup arr j'
    where j' = next j


-----------------------------------------------------------------------------------------------------------------------
-- Section 5. Using mutability to construct a fixed memoization table in ST
-----------------------------------------------------------------------------------------------------------------------

-- There can't be anything specific about the Collatz length function that makes the above work, so we abstract away
-- the function definition. Since we're now working inside a monad to update the mutable vector, we need a way of
-- doing the recursion in a monad. For Collatz all we need is applicative actually.

-- Collatz inside an applicative with the recursive calls performed by an applicative f.
collatzLengthApplicative :: Applicative f => ((Int -> f Int) -> Int -> f Int)
collatzLengthApplicative _ 1 = pure 1
collatzLengthApplicative f n = (+1) <$> f (next n)

-- To recover the naive solution inside an applicative, the recursive calls are to itself.
naiveCollatzLengthApplicative :: Applicative f => Int -> f Int
naiveCollatzLengthApplicative = collatzLengthApplicative naiveCollatzLengthApplicative

-- To really recover the recursive solution we use the Identity applicative which is inert.
-- Do we get anything interesting if we use other functors/applicatives/monads...?
naiveCollatzLength2 :: Int -> Int
naiveCollatzLength2 = runIdentity . naiveCollatzLengthApplicative

-- The generic version of a lookup function that gracefully recurses if the requested input is out of bounds or hasn't
-- yet been calculated, and when new in-bounds values are computed it writes them to the array. There are various ways
-- to mess with the code below like defining recur = applicativeF go i to avoid some repetition. What reads the best?
mutableLookup :: (Ix i, MArray a e m, Eq e) => ((i -> m e) -> i -> m e) -> e -> a i e -> i -> m e
mutableLookup applicativeF nil arr j = do
    memBounds <- MA.getBounds arr
    if inRange memBounds j then do
        x <- MA.readArray arr j
        when (x == nil) $ do
            y <- applicativeF go j -- recurse if the value for i hasn't been computed yet
            MA.writeArray arr j y
        MA.readArray arr j
    else applicativeF go j -- recurse if i is out of bounds 
    where go = mutableLookup applicativeF nil arr

-- The wrapper of the generic mutable lookup has a fearsome type signature that requries some language extensions. 
-- For this version, we might have asked for the value for some j outside the memoization bounds, so before leaving
-- the ST computation we write the desired value to the first index k in the array. This is kind of weird, no? A
-- more pleasing solution would be to compute the desired value in an ST context that can manage its own array.
memoizeRangeST :: (Ix i, Eq e) => (i, i) -> (forall f. Applicative f => (i -> f e) -> i -> f e) -> e -> i -> e
memoizeRangeST memBounds applicativeF nil j = arr IA.! k
    where
        k = fst memBounds
        arr = runSTArray $ do
            stArr <- MA.newArray memBounds nil
            x <- mutableLookup applicativeF nil stArr j
            MA.writeArray stArr k x
            return stArr

-- For reasons I do not fully understand, some extra constraints are needed for the STUArray version
memoizeRangeSTU :: forall i e. (Ix i, Eq e, IArray UArray e, forall s. MArray (STUArray s) e (ST s)) => (i, i) -> (forall f. Applicative f => (i -> f e) -> i -> f e) -> e -> i -> e
memoizeRangeSTU memBounds applicativeF nil j = arr IA.! k
    where
        k = fst memBounds
        arr :: UArray i e
        arr = runSTUArray $ do
            stArr <- MA.newArray memBounds nil
            x <- mutableLookup applicativeF nil stArr j
            MA.writeArray stArr k x
            return stArr

-- This version is vestigial, but was necessary for me to figure out the type signature of memoizeRangeST
collatzLengthMemoSTVestigial :: Int -> Int -> Int
collatzLengthMemoSTVestigial maxMemo j = arr IA.! k
    where
        memBounds = (1, maxMemo)
        nil = -1
        k = fst memBounds
        arr = runSTArray $ do
            stArr <- MA.newArray memBounds nil
            x <- mutableLookup collatzLengthApplicative nil stArr j
            MA.writeArray stArr k x
            return stArr

collatzLengthMemoSTGeneric :: Int -> Int -> Int
collatzLengthMemoSTGeneric maxMemo = memoizeRangeST (1, maxMemo) collatzLengthApplicative (-1)

collatzLengthMemoSTUGeneric :: Int -> Int -> Int
collatzLengthMemoSTUGeneric maxMemo = memoizeRangeSTU (1, maxMemo) collatzLengthApplicative (-1)


-----------------------------------------------------------------------------------------------------------------------
-- Section 6. Keeping the table for use later
-----------------------------------------------------------------------------------------------------------------------

-- In this Euler problem we want to find the j in [1..999999] with the largest collatzLength. The next thing to do is 
-- figure out how to maintain access to the memoization array rather than just writing a value to it and leaving it be.

computeMemoizedRangeST :: (Ix i, Eq e) => (i, i) -> (forall f. Applicative f => (i -> f e) -> i -> f e) -> e -> Array i e
computeMemoizedRangeST memBounds applicativeF nil = arr
    where
        k = fst memBounds
        arr = runSTArray $ do
            stArr <- MA.newArray memBounds nil
            forM_ (range memBounds) $ \j -> do 
                x <- mutableLookup applicativeF nil stArr j
                MA.writeArray stArr k x
            return stArr

computeMemoizedRangeSTU :: forall i e. (Ix i, Eq e, forall s. MArray (STUArray s) e (ST s)) => (i, i) -> (forall f. Applicative f => (i -> f e) -> i -> f e) -> e -> UArray i e
computeMemoizedRangeSTU memBounds applicativeF nil = arr
    where
        k = fst memBounds
        arr = runSTUArray $ do
            stArr <- MA.newArray memBounds nil
            forM_ (range memBounds) $ \j -> do 
                x <- mutableLookup applicativeF nil stArr j
                MA.writeArray stArr k x
            return stArr

-- takes about 0.1s
solve :: Int -> Int
solve n = fst . maximumBy (comparing snd) $ IA.assocs (computeMemoizedRangeSTU (1, n-1) collatzLengthApplicative (-1))

main :: IO ()
main = print $ solve 1000000