
module ArrayUtils (amapWithIxs) where

import Data.Array.IArray

amapWithIxs :: (IArray a e, IArray a e', Ix i) => (i -> e -> e') -> a i e -> a i e'
amapWithIxs f arr = array (bounds arr) (map (\i -> (i, f i (arr ! i))) (indices arr))
--runST (do
--		mArr <- newArray_
--		
--	)

