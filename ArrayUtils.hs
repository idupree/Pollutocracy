
module ArrayUtils (amapWithIxs, arraySize) where

import Data.Array.IArray

amapWithIxs :: (IArray a e, IArray a e', Ix i) => (i -> e -> e') -> a i e -> a i e'
amapWithIxs f arr = array (bounds arr) (map (\i -> (i, f i (arr ! i))) (indices arr))

arraySize :: (IArray a e) => a (Int,Int) e -> (Int,Int)
arraySize arr = case bounds arr of ((x1,y1),(x2,y2)) -> (x2 - x1 + 1, y2 - y1 + 1)
--runST (do
--		mArr <- newArray_
--		
--	)

