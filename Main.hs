
module Main(main) where

import Graphics.UI.GLUT
import Data.IORef
import qualified Sim
import System.Random
--as Random
import Control.Monad
import Data.Array
--import Data.List(intersperse)
import Data.Word(Word32)
import qualified SDL_Constants as SDL
import Data.Bits( (.|.) )
import Control.Arrow( (***) )
import System.Console.GetOpt
--import Data.Maybe(listToMaybe)
-- these are used for 'time'
import System.Time
import Control.Exception(bracket)

import qualified Display(doDisplay, initDisplay)

foreign import ccall unsafe "SDL.h SDL_Init" sdlInit :: Word32 -> IO ()
initTimeStuff :: IO ()
initTimeStuff = sdlInit (SDL.init_timer .|. SDL.init_noparachute)
foreign import ccall unsafe "SDL.h SDL_GetTicks" millisecondsNow :: IO Word32

-- ought to SDL_Init SDL_INIT_TIMER(a macro :( ) first
-- SDL_INIT_TIMER is a macro so it is only practically accessible from C
-- or is it? -cpp, {-\n#include "SDL.h"\n-}, SDL_INIT_TIMER...
-- since '{-' and '-}' aren't valid C (except inside strings, and it's unlikely
-- especially in a header file, and especially unbalanced ones)
--foreign import ccall unsafe "time/time_init.h initTimeStuff" initTimeStuff :: IO ()

--type WorldInfo = (Sim.World, ClockTime)

doTimedStuff :: IORef (Sim.World,Word32) -> IO ()
doTimedStuff worldRef = do
	ms <- millisecondsNow
	modifyIORef worldRef (Sim.simulate *** const ms)

doIdleStuff :: Int -> IORef (Sim.World,Word32) -> IO ()
doIdleStuff msPerStep worldRef = Display.doDisplay msPerStep worldRef --return ()

{-wastePicoseconds :: Integer -> IO ()
wastePicoseconds i = do
	startTime <- getClockTime
	let wasteMore = do
			thisTime <- getClockTime  --do picoseconds include everything?
			when (tdPicosec (diffClockTimes thisTime startTime) < i) wasteMore
		in wasteMore
-}
time :: IO a -> IO a
time = bracket getClockTime (\startTime -> do
		endTime <- getClockTime
		let diff = diffClockTimes endTime startTime
		putStr $ show $ diff; putChar ' '; print (tdPicosec diff `div` 1000000000)
	) . const
{-time io = do
	startTime <- getClockTime
	result <- io
	endTime <- getClockTime
	let diff = diffClockTimes endTime startTime
	putStr $ show $ diff; putChar ' '; print (tdPicosec diff `div` 1000000000)
	return result-}

initialWorld :: RandomGen g => g -> Sim.World
initialWorld g =
	Sim.World (
	    listArray ((0,0),(15,15)) $ map (\r ->
		if r < 4 then Just $ Sim.Generator (toEnum r) 5
		else if r < 16 then Just $ Sim.Mirror (toEnum (r `mod` 2)) (r<12) (r>=8)
		else Nothing
		) $ randomRs (0,99) g
	) (
	    []
	)
--Random.split >>> Arrow.first (\g ->

data Flag = MillisecondsPerStep Int

defaultMillisecondsPerStep :: Int
defaultMillisecondsPerStep = 500

options :: [OptDescr Flag]
options = [
	Option "s" ["milliseconds-per-step","ms/st"] (ReqArg (MillisecondsPerStep . read) "TIME")
		("Amount of TIME between simulation steps (default "++show defaultMillisecondsPerStep++")")
	
	]

main :: IO ()
main = do
	initTimeStuff
	firstWorld <- liftM initialWorld newStdGen
	worldRef <- newIORef (firstWorld,0)
	initialDisplayMode $= [DoubleBuffered]
	initialWindowSize $= Size 600 600
	(_programName, args) <- getArgsAndInitialize
	let (opts, _nonopts, optErrors) = getOpt Permute options args
	when (not (null optErrors)) (putStrLn "do something smarter than printing this")
	let msPerStep = case reverse [m | MillisecondsPerStep m <- opts] of
		(m:_) -> m
		[] -> defaultMillisecondsPerStep
	let triggerTimer = addTimerCallback msPerStep (doTimedStuff worldRef >> triggerTimer) in triggerTimer
	idleCallback $= Just (doIdleStuff msPerStep worldRef)
	createWindow "simulation"; Display.initDisplay; displayCallback $= Display.doDisplay msPerStep worldRef
	mainLoop

