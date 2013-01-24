{-# LANGUAGE ForeignFunctionInterface #-}
module Clock (millisecondsNow) where
import Data.Word (Word32)
import System.Posix.Clock

millisecondsNow :: IO Word32
millisecondsNow = fmap timeSpecToMilliseconds (getTime Monotonic)

timeSpecToMilliseconds :: (Integral i) => TimeSpec -> i
-- We place these fromIntegrals being cautious about not overflowing where possible,
-- for any i (timespec contains Ints, however oddly)
timeSpecToMilliseconds t = (fromIntegral (sec t) * 1000)
                           + fromIntegral (nsec t `div` 1000000)

