-- License: GPL v2 or later
{-# LANGUAGE ForeignFunctionInterface #-}

module Display(doDisplay, initDisplay) where

import Graphics.UI.GLUT
import qualified Sim
import Data.Array.Unboxed
import Data.Word(Word32)
import Control.Monad(when, unless, liftM2, forM_)
import System.Random
import ArrayUtils (arraySize)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CFloat(CFloat))

-- This C code is for speed.  I first wrote this code before vertex buffers
-- even existed in the OpenGL standard (and before I knew about them);
-- it's quite likely that modern GHC and OpenGL (ES) 2.0 could be plenty fast
-- without using C code.
foreign import ccall unsafe "foreignPollution" foreignPollution :: Word32 -> Ptr Double -> Word32 -> Word32 -> IO ()

-- We don't use the Random instance of GLfloat (CFloat) directly
-- because it uses realToFrac which might be slow.
randomRIOGLF :: (Float, Float) -> IO GLfloat
randomRIOGLF range_ = do
  result <- randomRIO range_
  return (CFloat result)

initDisplay :: IO ()
initDisplay = do
	ortho2D 0 1 0 1
	blend $= Enabled ; blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

-- Possibly the three int-like arguments should be better distinguished
-- or combined ("data TimeSituation"?) somehow.  Or even made into one
-- value, simStepsSinceLastUpdate(Float), which could be used fine as a random
-- seed (which ms is currently used for), most likely (just transform it appropriately).
-- TODO.
doDisplay :: Sim.World -> Int{-milliseconds per step-}
	-> Word32{-current time-} -> Word32{-last update time-} -> IO ()
doDisplay (Sim.World worldMap worldParticles worldCreatures worldPollution worldHour)
  msPerStep ms lastUpdateTime = do
	let simStepsSinceLastUpdate = fromIntegral (ms - lastUpdateTime) / fromIntegral msPerStep :: GLfloat
	let ((minX,minY),(maxX,maxY)) = bounds worldMap
	let (numX,numY) = (fromIntegral $ maxX + 1 - minX, fromIntegral $ maxY + 1 - minY)
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
		    --hmm, could separate list by particle-type and
		    --encompass more with each renderPrimitive...
		    case pType of
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
			Sim.Mirror mdir lSilvered rSilvered -> do
				rotate (case mdir of { Sim.NW_SE -> 0; Sim.SW_NE -> -90 } :: GLfloat) rotationReference
				let
					dist = 0.25 :: GLfloat
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
			Sim.Riverbed{} -> error "Unimplemented yet"

		-- draw the pollution!
		-- (the 'resolution' should depend on windowsize / (i.e.?) number of places displayed)
		-- currently just 5x5 per place though
		-- should it be invisible where really low?
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
			renderPrimitive Quads $ do
				vertex $ Vertex2 0 (0::GLfloat)
				vertex $ Vertex2 numX (0::GLfloat)
				vertex $ Vertex2 numX numY
				vertex $ Vertex2 0 numY

	swapBuffers
	reportErrors

