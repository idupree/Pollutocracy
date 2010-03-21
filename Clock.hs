{-# LANGUAGE ForeignFunctionInterface #-}
--{-# INCLUDE <SDL.h> #-}
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

--for initializing: although it might need to be combined with other SDL flags
--if SDL were used for other things "too":
--import qualified SDL_Constants as SDL
--import Data.Bits( (.|.) )
--foreign import ccall unsafe "SDL.h SDL_Init" sdlInit :: Word32 -> IO ()
--initTimeStuff :: IO ()
--initTimeStuff = sdlInit (SDL.init_timer .|. SDL.init_noparachute)


-- an older attempt, that was in Display, that I guess I dropped
-- for some reason... (probably it didn't work)
{-
import System.Time
time' :: String -> IO a -> IO a
time' str = bracket getClockTime (\startTime -> do
		endTime <- getClockTime
		let diff = diffClockTimes endTime startTime
		putStr str
		putStr $ show $ tdPicosec diff `div` 1000000000
		putChar '.'; putStr $ tail $ show $ 10000 + (tdPicosec diff `div` 1000000)
		--putChar '.'; putStr $ show $ tdPicosec diff `div` 1000000 `mod` 1000; 
		putStr " ms  {"; putStr $ drop 60 $ show $ diff; putChar '\n'
	) . const

time :: IO a -> IO a
time = time' ""

-- L for Label
timeL :: String -> IO a -> IO a
timeL str = time' (str++": ")
-}


--other weird stuff that was in main.
--Substitute for real profiling??--ghc's -prof -auto-all is alright(and could add
--mid-'display'-func SCC pragma perhaps)
--(and for opengl testing)
--(well, this computer is too fast so i dunno anyway)
{-
-- these are used for 'time'
import System.Time
import Control.Exception(bracket)
time :: IO a -> IO a
time = bracket getClockTime (\startTime -> do
		endTime <- getClockTime
		let diff = diffClockTimes endTime startTime
		putStr $ show $ diff; putChar ' '; print (tdPicosec diff `div` 1000000000)
	) . const
-}
{-wastePicoseconds :: Integer -> IO ()
wastePicoseconds i = do
	startTime <- getClockTime
	let wasteMore = do
			thisTime <- getClockTime  --do picoseconds include everything?
			when (tdPicosec (diffClockTimes thisTime startTime) < i) wasteMore
		in wasteMore
-}
{-time io = do
	startTime <- getClockTime
	result <- io
	endTime <- getClockTime
	let diff = diffClockTimes endTime startTime
	putStr $ show $ diff; putChar ' '; print (tdPicosec diff `div` 1000000000)
	return result-}

