{-# LANGUAGE ForeignFunctionInterface #-}
module Clock (millisecondsNow) where
import Data.Word (Word32)

import System.Posix.Clock

millisecondsNow :: IO Word32
millisecondsNow = fmap timeSpecToMilliseconds (getTime Monotonic)

timeSpecToMilliseconds :: (Integral i) => TimeSpec -> i
-- place fromIntegrals being cautious about not overflowing where possible, for any i
-- (timespec contains Ints, however oddly)
timeSpecToMilliseconds t = (fromIntegral (sec t) * 1000) + fromIntegral (nsec t `div` 1000000)

-- SDL catches Ctrl-C whether or not you initialize it or use SDL_NOPARACHUTE
-- (it wants you to use its event loop and catch SDL_Quit)
-- so, out with da evil sdl!
--foreign import ccall unsafe {-"SDL.h-} "SDL_GetTicks" millisecondsNow :: IO Word32

