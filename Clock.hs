{-# LANGUAGE ForeignFunctionInterface #-}
module Clock (millisecondsNow) where
import Data.Word (Word32)

foreign import ccall unsafe {-"SDL.h-} "SDL_GetTicks" millisecondsNow :: IO Word32

