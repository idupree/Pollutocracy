-- Some experiments from before I understood Haskell performance as well
-- (and a few years ago so GHC's optimizer wasn't as good).
-- These days I'd probably take a similar approach to the C code:
-- avoid type classes, just provide some simple inlineable functions to
-- step the RNG: and it could probably rival the C's performance.
module FastRoughRNG(FastRoughRNG(..), module System.Random, roughFloatingRange,
	randomFloatingList, randomFloatingArray, randomFloatingArray2) where

import System.Random
import Data.Bits
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (unless)
import Control.Monad.Fix (fix)
--import Data.List (mapAccumL)
--import Data.Word (Word)

--  == http://www.gnu.org/software/gsl/manual/gsl-ref_17.html#SEC278 gsl_rng_rand
newtype FastRoughRNG = MkFastRoughRNG Int --Word

instance RandomGen FastRoughRNG where
	next (MkFastRoughRNG n) = (n', MkFastRoughRNG n')
		where n' = (1103515245 * n + 12345) .&. 0x7fffffff
	split = undefined  -- :( I don't know how I would implement this correctly
	genRange _ = (0,0x7fffffff)
	--{-# INLINE genRange #-}
	--{-# INLINE next #-}

roughFloatingRange :: (Double,Double) -> FastRoughRNG -> (Double, FastRoughRNG)
roughFloatingRange (minr,maxr) rng = (n', rng')
	where
		(n, rng') = next rng
		n' = minr + fromIntegral n * (maxr - minr) / 0x7fffffff

randomFloatingList :: Int -> FastRoughRNG -> ([Double], FastRoughRNG)
randomFloatingList 0 rng = ([],rng)
randomFloatingList count rng = n' `seq` (n' : ns, rng'')
	where
		(n,rng') = next rng
		n' = (fromIntegral n / 0x7fffffff)
		(ns,rng'') = randomFloatingList (count-1) rng'

randomFloatingArray :: (Int,Int) -> FastRoughRNG -> (UArray Int Double, FastRoughRNG)
randomFloatingArray bound@(i1,i2) rng = runST (do
			mArr <- newArray_ bound :: ST s (STUArray s Int Double)
			{-let set :: Int -> FastRoughRNG -> ST s FastRoughRNG
			    set i rng' = do
					writeArray mArr i n'
					if i /= i2 then set (i+1) rng'' else return rng''
				where
					(n,rng'') = next rng'
					n' = fromIntegral n / 0x7fffffff-}
			rngFinal <- (fix (\set i rng' -> do
					let (n,rng'') = next rng'
					let n' = fromIntegral n / 0x7fffffff
					writeArray mArr i n'
					if i /= i2 then set (i+1) rng'' else return rng''
				)) i1 rng
			arrFinal <- unsafeFreeze mArr
			return (arrFinal, rngFinal)
		)

randomFloatingArray2 :: ((Int,Int),(Int,Int)) -> FastRoughRNG -> (UArray (Int,Int) Double, FastRoughRNG)
randomFloatingArray2 bound@(i1@(_,y1),i2@(_,y2)) rng = runST (do
			mArr <- newArray_ bound :: ST s (STUArray s (Int,Int) Double)
			let set i@(x,y) rng' = do
					writeArray mArr i n'
					if i /= i2
					 then set (let yplus1 = y+1 in if yplus1 <= y2 then (x,yplus1) else (x+1,y1)) rng''
					 else return rng''
				where
					(n,rng'') = next rng'
					n' = fromIntegral n / 0x7fffffff
			rngFinal <- set i1 rng
			arrFinal <- unsafeFreeze mArr
			return (arrFinal, rngFinal)
		)

-- \(x,y) -> if (y+1) <= y2 then (x,y+1) else (x+1,y1)
		
{-	case next rng of { (n,rng') ->
	let n' = (fromIntegral n / 0x7fffffff) in  n' `seq` (
	
	n' : randomFloatingList (count-1) rng' )}
-}
--randomFloatingArray :: (Ix i) => (i,i) -> FastRoughRNG -> (UArray i Double, FastRoughRNG)
--randomFloatingArray bound rng = (array ns, rng''')
--	where (ns,rng''') = mapAccumL (\rng' ix -> case next rng' of
--						(n,rng'') -> ((ix, fromIntegral n / 0x7fffffff), rng''))
--				rng (range bound)

