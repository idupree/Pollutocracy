{-# LANGUAGE ForeignFunctionInterface, FlexibleContexts #-}
-- deprecated: no longer has any effect: {-# INCLUDE "foreignPollution.h" #-}
module Display(doDisplay, initDisplay) where

import Graphics.UI.GLUT
--import Data.IORef
import qualified Sim
import Clock (millisecondsNow)
--as Random
import Data.Array.Unboxed
--import Data.List(intersperse)
import Data.Word(Word32)
import Control.Monad(when, unless, liftM2, forM_)
--import FastRoughRNG
--import Control.Arrow ( (***) )
--import qualified Data.Array.IO as IOArr
import System.Random
import ArrayUtils (arraySize)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr)
import Unsafe.Coerce (unsafeCoerce) --lame...
foreign import ccall unsafe "foreignPollution" foreignPollution :: Word32 -> Ptr Double -> Word32 -> Word32 -> IO ()

--since realToFrac and that crap, and non-exposed newtype
randomRIOGLF :: (Float, Float) -> IO GLfloat
randomRIOGLF range_ = do
  result <- randomRIO range_
  return (unsafeCoerce result)

initDisplay :: IO ()
initDisplay = do
	ortho2D 0 1 0 1
	blend $= Enabled ; blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

-- I should refactor this to have type
-- Sim.World -> Word32{-current time-} -> Word32{-last update time-} -> IO ().
-- There is no excuse for the current signature.
doDisplay :: Int -> IO (Sim.World,Word32) -> IO ()
doDisplay msPerStep getWorld = do
	ms <- millisecondsNow
	--newColor <- liftM4 Color4 (randomRIOGLF (0,1)) (randomRIOGLF (0,1)) (randomRIOGLF (0,1)) (randomRIOGLF (1,1))
	--clearColor $= newColor
	--clear [ColorBuffer] -- --make random colors
	(Sim.World worldMap worldParticles worldCreatures worldPollution worldHour, lastUpdateTime) <- getWorld
	let simStepsSinceLastUpdate = fromIntegral (ms - lastUpdateTime) / fromIntegral msPerStep :: GLfloat
	--print worldMap
	let ((minX,minY),(maxX,maxY)) = bounds worldMap
	let (numX,numY) = {-case bounds worldMap of ((minX,maxX),(minY,maxY)) ->-} (fromIntegral $ maxX + 1 - minX, fromIntegral $ maxY + 1 - minY)
	--print (numX,numY)
	-- draw the floors
	{-dl <- defineNewList Compile $ renderPrimitive Quads $ do
			color (Color3 0.4 0.5 0.7 :: Color3 GLfloat); vertex (Vertex2 0 0 :: Vertex2 GLfloat)
			color (Color3 0.4 0.6 0.4 :: Color3 GLfloat); vertex (Vertex2 1 0 :: Vertex2 GLfloat)
			color (Color3 0.7 0.7 0.5 :: Color3 GLfloat); vertex (Vertex2 1 1 :: Vertex2 GLfloat)
			color (Color3 0.4 0.3 0.4 :: Color3 GLfloat); vertex (Vertex2 0 1 :: Vertex2 GLfloat)
	time $ preservingMatrix $ do
		scale (1/numX) (1/numY::GLfloat) 1
		mapM_ (\ ((x,y),maybeMachine) -> let (x',y') = (fromIntegral x :: GLfloat, fromIntegral y :: GLfloat) in do
			unsafePreservingMatrix $ do
				translate (Vector3 x' y' 0)
				callList dl) $ assocs worldMap-}
	let rotationReference = Vector3 0 0 1 :: Vector3 GLfloat
	let distFromCenter = 0.5 :: GLfloat
	let translatingTo :: Integral coord => (coord,coord) -> IO a -> IO a
	    translatingTo (x,y) io = preservingMatrix $ do
			translate (Vector3 (fromIntegral x + distFromCenter) (fromIntegral y + distFromCenter) 0 :: Vector3 GLfloat)
			io
	preservingMatrix $ do
		scale (recip numX) (recip numY :: GLfloat) 1
		-- draw the floors
		--timeL "flor" $ 
		renderPrimitive Quads $ forM_ (assocs worldMap) $ \ ((x,y), _maybeMachine) -> do
			let (x',y') = (fromIntegral x :: GLfloat, fromIntegral y :: GLfloat)
			color (Color3 0.50 0.50 0.60 :: Color3 GLfloat); vertex (Vertex2  x'     y'    )
			color (Color3 0.45 0.55 0.45 :: Color3 GLfloat); vertex (Vertex2 (x'+1)  y'    )
			color (Color3 0.60 0.60 0.50 :: Color3 GLfloat); vertex (Vertex2 (x'+1) (y'+1) )
			color (Color3 0.40 0.35 0.40 :: Color3 GLfloat); vertex (Vertex2  x'    (y'+1) )
		-- draw the creatures (what about multiple creatures in the same place?
		-- I guess one just gets drawn first, then the next...)
		--timeL "cret" $ 
		forM_ worldCreatures $ \ (loc, creature) -> translatingTo loc $ do
		    case creature of
			Sim.Water _rng -> do
				color (Color4 0.4 0.5 0.9 0.6 :: Color4 GLfloat)
				renderPrimitive Quads $ do
					let shortDist = 0.5 :: GLfloat
					let longDist = 0.5 :: GLfloat
					vertex $ Vertex2 (-longDist) (-shortDist)
					vertex $ Vertex2 ( longDist) (-shortDist)
					vertex $ Vertex2 ( longDist) ( shortDist)
					vertex $ Vertex2 (-longDist) ( shortDist)
			Sim.Creature { Sim.creatureEnergy = energy } -> do
				renderPrimitive Quads $ do
					let legSpread = 0.35 :: GLfloat
					let legHeight = -0.4 :: GLfloat
					let waistSpread = 0.2 :: GLfloat
					let waistHeight = 0 :: GLfloat
					let armSpread = 0.3 :: GLfloat
					let shoulderSpread = 0.2 :: GLfloat
					let neckSpread = 0.1 :: GLfloat
					let neckHeight = 0.2 :: GLfloat
					let headHeight = 0.45 :: GLfloat
					let headSpread = 0.2 :: GLfloat
					color (Color3 0.5 0.5 (0.7 + realToFrac energy / 20) :: Color3 GLfloat)
					vertex $ Vertex2 (-legSpread) (legHeight)
					vertex $ Vertex2 ( legSpread) (legHeight)
					vertex $ Vertex2 ( waistSpread) (waistHeight)
					vertex $ Vertex2 (-waistSpread) (waistHeight)
					color (Color3 0.4 0.9 0.6 :: Color3 GLfloat)
					vertex $ Vertex2 (-armSpread) (waistHeight)
					vertex $ Vertex2 ( armSpread) (waistHeight)
					vertex $ Vertex2 ( shoulderSpread) (neckHeight)
					vertex $ Vertex2 (-shoulderSpread) (neckHeight)
					color (Color3 0.9 0.7 0.5 :: Color3 GLfloat)
					vertex $ Vertex2 (-neckSpread) (neckHeight)
					vertex $ Vertex2 ( neckSpread) (neckHeight)
					vertex $ Vertex2 ( headSpread) (headHeight)
					vertex $ Vertex2 (-headSpread) (headHeight)
		-- draw the particles (what relation to machines? what about the dangerous particles?)
		--timeL "part" $ 
		forM_ worldParticles $ \ (loc, Sim.Particle dir pType) -> translatingTo loc $ do
		    rotate (Sim.dirAngle dir) rotationReference
		    translate (Vector3 (simStepsSinceLastUpdate) 0 0)
		    case pType of  --hmm, could separate list by particle-type and encompass more with each renderPrimitive...
		    	Sim.Energy strength -> do
				color (Color4 0.9 0.9 0.2 (log $ fromIntegral strength) :: Color4 GLfloat)
				renderPrimitive Quads $ do
					let shortDist = 0.15 :: GLfloat
					let longDist = 0.25 :: GLfloat
					vertex $ Vertex2 (-longDist) (-shortDist)
					vertex $ Vertex2 ( longDist) (-shortDist)
					vertex $ Vertex2 ( longDist) ( shortDist)
					vertex $ Vertex2 (-longDist) ( shortDist)
			-- could draw this after the machines...
			Sim.Chaos _rng -> do
				let
					io :: IO ()
					io = do
						alpha1 <- randomRIOGLF (0.7,1.0)
						color (Color4 0.7 0.2 0.7 alpha1 :: Color4 GLfloat)
						let randPos = randomRIOGLF (-0.5, 0.5)
						let randVertex = vertex =<< liftM2 Vertex2 randPos randPos
						randVertex ; randVertex ; randVertex
						randVal <- randomRIO (1, 10 :: Int)
						unless (randVal == 1) io
				renderPrimitive Triangles $ io
{-
			Sim.Water _rng -> do --HACK!
		    		translate (Vector3 (negate simStepsSinceLastUpdate) 0 0)
				color (Color4 0.4 0.5 0.9 0.3 :: Color4 GLfloat)
				renderPrimitive Quads $ do
					let shortDist = 0.5 :: GLfloat
					let longDist = 0.5 :: GLfloat
					vertex $ Vertex2 (-longDist) (-shortDist)
					vertex $ Vertex2 ( longDist) (-shortDist)
					vertex $ Vertex2 ( longDist) ( shortDist)
					vertex $ Vertex2 (-longDist) ( shortDist)
-}
				
		-- draw the machines
		--timeL "mach" $ 
		forM_ [(l,m) | (l,Just m) <- assocs worldMap] $ \ (loc,machine) -> translatingTo loc $ do
		    case machine of
			Sim.Generator dir energy -> do
				rotate (Sim.dirAngle dir) (Vector3 0 0 1 :: Vector3 GLfloat)
				renderPrimitive Quads $ do
					let dist = 0.3 :: GLfloat
					color (Color3 0.6 0.6 0.6 :: Color3 GLfloat)
					vertex $ Vertex2 (-dist) (-dist)
					vertex $ Vertex2 ( dist) (-dist)
					vertex $ Vertex2 ( dist) ( dist)
					vertex $ Vertex2 (-dist) ( dist)
					let shortDist = 0.1 :: GLfloat
					let yellow = fromIntegral energy / 10
					color (Color3 yellow yellow 0.0 :: Color3 GLfloat)
					vertex $ Vertex2 (-shortDist) (-shortDist)
					vertex $ Vertex2 ( dist) (-shortDist)
					vertex $ Vertex2 ( dist) ( shortDist)
					vertex $ Vertex2 (-shortDist) ( shortDist)
			Sim.Mirror mdir lSilvered rSilvered -> do --mirrorSilveredWhenGoingDirection
				rotate (case mdir of { Sim.NW_SE -> 0; Sim.SW_NE -> -90 } :: GLfloat) rotationReference
				let
					--colorBy False = color (Color3 0.9 0.9 0.9 :: Color3 GLfloat)
					--colorBy True  = color (Color3 0.7 0.7 0.7 :: Color3 GLfloat)
					--xMult = case mdir of { Sim.NW_SE -> 1; Sim.SW_NE -> -1 }
					dist = 0.25 :: GLfloat
					--avgDist = (distFromCenter + dist) / 2
					silverDepth = 0.07 -- orthogonal, in both directions
				renderPrimitive Quads $ do
					color (Color3 0.9 0.9 0.9 :: Color3 GLfloat)
					vertex $ Vertex2 (-dist) (distFromCenter)
					vertex $ Vertex2 (distFromCenter) (-dist)
					vertex $ Vertex2 (dist) (-distFromCenter)
					vertex $ Vertex2 (-distFromCenter) (dist)
					
					color (Color3 0.7 0.7 0.8 :: Color3 GLfloat)
					when lSilvered $ do
						vertex $ Vertex2 (-(distFromCenter-silverDepth)) (dist+silverDepth)
						vertex $ Vertex2 (dist+silverDepth) (-(distFromCenter-silverDepth))
						vertex $ Vertex2 (dist) (-distFromCenter)
						vertex $ Vertex2 (-distFromCenter) (dist)
					
					when rSilvered $ do
						vertex $ Vertex2 (-dist) (distFromCenter)
						vertex $ Vertex2 (distFromCenter) (-dist)
						vertex $ Vertex2 (distFromCenter-silverDepth) (-(dist+silverDepth))
						vertex $ Vertex2 (-(dist+silverDepth)) (distFromCenter-silverDepth)
			Sim.Greenery -> do
				renderPrimitive Polygon $ do
					color (Color3 0.2 0.9 0.3 :: Color3 GLfloat)
					vertex $ Vertex2 (-0.3) (0.2 :: GLfloat)
					vertex $ Vertex2 (0) (distFromCenter)
					vertex $ Vertex2 (0.3) (0.2 :: GLfloat)
					vertex $ Vertex2 (0.5) (-0.3 :: GLfloat)
					vertex $ Vertex2 (0.3) (-0.4 :: GLfloat)
					vertex $ Vertex2 (0.1) (-0.35 :: GLfloat)
					vertex $ Vertex2 (-0.1) (-0.35 :: GLfloat)
					vertex $ Vertex2 (-0.3) (-0.4 :: GLfloat)
					vertex $ Vertex2 (-0.5) (-0.3 :: GLfloat)
				renderPrimitive Triangles $ do
					color (Color3 0.6 0.4 0.3 :: Color3 GLfloat)
					let trunkWidth = 0.1 :: GLfloat
					let trunkY = 0.25 :: GLfloat
					vertex $ Vertex2 (0) (trunkY)
					vertex $ Vertex2 (trunkWidth) (-distFromCenter)
					vertex $ Vertex2 (-trunkWidth) (-distFromCenter)
					let branchTipY = -0.1 :: GLfloat
					let branchTipX = 0.25 :: GLfloat
					let branchStartHigh = -0.3 :: GLfloat
					let branchStartLow = -0.4 :: GLfloat
					vertex $ Vertex2 (0) (branchStartHigh)
					vertex $ Vertex2 (branchTipX) (branchTipY)
					vertex $ Vertex2 (0) (branchStartLow)

					vertex $ Vertex2 (0) (branchStartHigh)
					vertex $ Vertex2 (-branchTipX) (branchTipY)
					vertex $ Vertex2 (0) (branchStartLow)
			Sim.Storm energy _rng -> do
				let
					io :: Double -> Int -> IO ()
					io amount side = do
						alpha1 <- randomRIOGLF (0.7,1.0)
						color (Color4 0.4 0.3 0.7 alpha1 :: Color4 GLfloat)
						let randPos a b = randomRIOGLF (a, b)
						let randVertex = [
							vertex =<< liftM2 Vertex2 (randPos 0 0.5) (randPos (-0.5) 0.5),
							vertex =<< liftM2 Vertex2 (randPos (-0.5) 0) (randPos (-0.5) 0.5),
							vertex =<< liftM2 Vertex2 (randPos (-0.5) 0.5) (randPos 0 0.5),
							vertex =<< liftM2 Vertex2 (randPos (-0.5) 0.5) (randPos (-0.5) 0)]
							 !! side
						randVertex ; randVertex ; randVertex
						unless (amount < 0) ( io (amount - 1) ( (side + 1) `mod` 4 ) )
				renderPrimitive Triangles $ io ((energy+2) * 4) 0
			Sim.Mountain -> do
				-- How about shadows i.e. "The sun rises in the east"?
				renderPrimitive Triangles $ do
					let width = 0.3 :: GLfloat
					let height = 0.4 :: GLfloat
					let offsetX = 0.15 :: GLfloat
					let offsetY = 0.35 :: GLfloat
					color (Color3 0.45 0.4 0.35 :: Color3 GLfloat)
					vertex $ Vertex2 ((-width)-offsetX) (offsetY-0.1)
					vertex $ Vertex2 (-offsetX) (height+offsetY-0.1)
					vertex $ Vertex2 (width-offsetX) (offsetY-0.1)
					color (Color3 0.25 0.2 0.15 :: Color3 GLfloat)
					vertex $ Vertex2 ((-width)+offsetX) (offsetY)
					vertex $ Vertex2 (offsetX) (height+offsetY)
					vertex $ Vertex2 (width+offsetX) (offsetY)
					color (Color3 0.35 0.4 0.45 :: Color3 GLfloat)
					vertex $ Vertex2 (-width) (0)
					vertex $ Vertex2 (0) (height)
					vertex $ Vertex2 (width) (0)
					color (Color3 0.2 0.2 0.2 :: Color3 GLfloat)
					vertex $ Vertex2 ((-width)-offsetX) (-offsetY)
					vertex $ Vertex2 (-offsetX) (height-offsetY)
					vertex $ Vertex2 (width-offsetX) (-offsetY)
					color (Color3 0.3 0.4 0.3 :: Color3 GLfloat)
					vertex $ Vertex2 ((-width)+offsetX) ((-offsetY)+0.05)
					vertex $ Vertex2 (offsetX) ((height-offsetY)+0.05)
					vertex $ Vertex2 (width+offsetX) ((-offsetY)+0.05)
					
			{-Sim.Mirror Sim.SW_NE -> do
				color (Color3 0.9 0.9 0.9 :: Color3 GLfloat)
				renderPrimitive Quads $ do
					let dist = 0.4 :: GLfloat
					vertex (Vertex2 (dist) (distFromCenter))
					vertex (Vertex2 (-distFromCenter) (-dist))
					vertex (Vertex2 (-dist) (-distFromCenter))
					vertex (Vertex2 (distFromCenter) (dist))-}

		-- draw the pollution!
		-- (the 'resolution' should depend on windowsize / (i.e.?) number of places displayed)
		-- currently just 5x5 per place though
		-- should it be invisible where really low?
		--mapM_ (\ (loc,thickness) -> translatingTo loc $ do
		do
			let (width, height) = arraySize worldPollution
			-- marshalling takes about 1 ms by last measurement
			withArray (elems worldPollution) (\cArr -> foreignPollution ms cArr (fromIntegral width) (fromIntegral height))
		-- draw the night-time! (er.) (HACK!!!)
		do
			let dayFraction = case worldHour of Sim.WorldHour h -> (realToFrac h + simStepsSinceLastUpdate) / realToFrac Sim.dayLength
			let dayLight = if dayFraction >= 0.5 then 0 else sin (dayFraction * pi * 2)
			let nightMasking = (1 - dayLight) / 2 /10--since it doesn't do anything
			color (Color4 0.1 0.1 0.3 nightMasking :: Color4 GLfloat)
			renderPrimitive Quads $ do--numX numY
				vertex $ Vertex2 0 (0::GLfloat)
				vertex $ Vertex2 numX (0::GLfloat)
				vertex $ Vertex2 numX numY
				vertex $ Vertex2 0 numY

	swapBuffers
	reportErrors

