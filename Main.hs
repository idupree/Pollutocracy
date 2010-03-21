{-# LANGUAGE ForeignFunctionInterface #-}

module Main(main) where

import Graphics.UI.GLUT
import Data.IORef
import qualified Sim
import System.Random
--as Random
import Control.Monad
import Data.Array.IArray
--import Data.List(intersperse)
import Data.Word(Word32)
--import qualified SDL_Constants as SDL
import Data.Bits( (.|.) )
import Control.Arrow( (***) )
import System.Console.GetOpt
--import Data.Maybe(listToMaybe)
-- these are used for 'time'
import System.Time
import Control.Exception(bracket)

import qualified Display(doDisplay, initDisplay)
import ArrayUtils (arraySize)
import Clock (millisecondsNow)

--foreign import ccall unsafe "SDL.h SDL_Init" sdlInit :: Word32 -> IO ()
initTimeStuff :: IO ()
initTimeStuff = return ()--sdlInit (SDL.init_timer .|. SDL.init_noparachute)

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
doIdleStuff msPerStep worldRef = Display.doDisplay msPerStep (readIORef worldRef) --return ()

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

randomRNGs :: RandomGen g => g -> [g]
randomRNGs rng = rng1 : randomRNGs rng2
	where (rng1,rng2) = split rng


initialWorld :: StdGen -> Sim.World
initialWorld g =
	let
		bound = ((0,0),(15,15))
	in Sim.World (
	    listArray bound $ map (\rng -> let (r,rng') = randomR (0,99) rng in
		if r < 4 then Just $ Sim.Generator (toEnum r) 5
		else if r < 16 then Just $ Sim.Mirror (toEnum (r `mod` 2)) (r<12) (r>=8)
		else if r < 25 then Just Sim.Greenery
		else if r < 27 then Just $ Sim.Storm 2 rng'
		else if r < 40 then Just Sim.Mountain
		else Nothing
		) $ randomRNGs g
	) (
	    []
	) (
	    [((6,6), Sim.Creature 4 g)]
	) (
	    listArray bound (repeat 0)
	)

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
	when (not (null optErrors)) (putStrLn "do something smarter than printing these errors?" >> mapM_ putStrLn optErrors)
	let msPerStep = case reverse [m | MillisecondsPerStep m <- opts] of
		(m:_) -> m
		[] -> defaultMillisecondsPerStep
	let triggerTimer = addTimerCallback msPerStep (doTimedStuff worldRef >> triggerTimer) in triggerTimer
	idleCallback $= Just (doIdleStuff msPerStep worldRef)
	createWindow "simulation"; do
		Display.initDisplay
		displayCallback $= Display.doDisplay msPerStep (readIORef worldRef)
		keyboardMouseCallback $= Just (clickCallback (readIORef worldRef))
	mainLoop

clickCallback :: IO (Sim.World,Word32) -> Key -> KeyState -> Modifiers -> Position -> IO ()
clickCallback getWorld (MouseButton MiddleButton) Down _mods pos = do
	--never mind being a useful gui-interface at the moment
	(x,y) <- positionToZeroOneRange pos  -- annoying. the window could theoretically have been resized by now.
	(Sim.World machines _particles _creatures pollutions, _) <- getWorld
	let (width,height) = arraySize machines
	let loc = (truncate (x * fromIntegral width), truncate (y * fromIntegral height))
	putStrLn $ show loc ++ ":\n\t" ++ show (machines!loc) ++ "\n\tPollution: " ++ show (pollutions!loc)
clickCallback _ _ _ _ _ = return ()

positionToZeroOneRange :: Position -> IO (Double,Double)
positionToZeroOneRange (Position x y) = get viewport >>=
	\(Position minX minY, Size sizeX sizeY) -> return
		(fromIntegral (x - minX) / fromIntegral sizeX,
		 fromIntegral (fromIntegral sizeY - (y - minY) {-stupid GLUT difference of *origin* to OpenGL-}) / fromIntegral sizeY)

