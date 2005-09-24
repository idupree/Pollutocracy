
module Display(doDisplay, initDisplay) where

import Graphics.UI.GLUT
import Data.IORef
import qualified Sim
--as Random
import Data.Array
--import Data.List(intersperse)
import Data.Word(Word32)
import Control.Monad(when)
foreign import ccall unsafe "SDL.h SDL_GetTicks" millisecondsNow :: IO Word32

initDisplay :: IO ()
initDisplay = do
	ortho2D 0 1 0 1
	blend $= Enabled ; blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

doDisplay :: Int -> IORef (Sim.World,Word32) -> IO ()
doDisplay msPerStep worldRef = do
	ms <- millisecondsNow
	--newColor <- liftM4 Color4 (randomRIO (0,1)) (randomRIO (0,1)) (randomRIO (0,1)) (randomRIO (1,1))
	--clearColor $= newColor
	--clear [ColorBuffer] -- --make random colors
	(Sim.World worldMap worldParticles, lastUpdateTime) <- readIORef worldRef
	let secondsSinceLastUpdate = fromIntegral (ms - lastUpdateTime) / fromIntegral msPerStep :: GLfloat
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
	preservingMatrix $ do
		scale (1/numX) (1/numY::GLfloat) 1
		-- draw the floors
		renderPrimitive Quads $ mapM_ (\ ((x,y), _maybeMachine) ->
		    let (x',y') = (fromIntegral x :: GLfloat, fromIntegral y :: GLfloat) in do
			color (Color3 0.5 0.5 0.6 :: Color3 GLfloat); vertex (Vertex2  x'     y'    )
			color (Color3 0.45 0.55 0.45 :: Color3 GLfloat); vertex (Vertex2 (x'+1)  y'    )
			color (Color3 0.6 0.6 0.5 :: Color3 GLfloat); vertex (Vertex2 (x'+1) (y'+1) )
			color (Color3 0.4 0.35 0.4 :: Color3 GLfloat); vertex (Vertex2  x'    (y'+1) )
		  ) $ assocs worldMap
		-- draw the particles (what relation to machines? what about the dangerous particles?)
		mapM_ (\ ((x,y), Sim.Particle dir pType) -> let (x',y') = (fromIntegral x :: GLfloat, fromIntegral y :: GLfloat) in
		  preservingMatrix $ do
		    translate (Vector3 (x'+0.5) (y'+0.5) 0)
		    rotate (Sim.dirAngle dir) rotationReference
		    translate (Vector3 (secondsSinceLastUpdate) 0 0)
		    case pType of
		    	Sim.Energy strength -> do
				color (Color4 0.9 0.9 0.2 (log $ fromIntegral strength) :: Color4 GLfloat)
				renderPrimitive Quads $ do
					let shortDist = 0.15 :: GLfloat
					let longDist = 0.25 :: GLfloat
					vertex (Vertex2 (-longDist) (-shortDist))
					vertex (Vertex2 ( longDist) (-shortDist))
					vertex (Vertex2 ( longDist) ( shortDist))
					vertex (Vertex2 (-longDist) ( shortDist))
		  ) worldParticles
		-- draw the machines
		mapM_ (\ ((x,y),machine) -> let (x',y') = (fromIntegral x :: GLfloat, fromIntegral y :: GLfloat) in
		  preservingMatrix $ do
		    translate (Vector3 (x'+0.5) (y'+0.5) 0)
		    let distFromCenter = 0.5 :: GLfloat
		    case machine of
			Sim.Generator dir energy -> do
				rotate (Sim.dirAngle dir) (Vector3 0 0 1 :: Vector3 GLfloat)
				renderPrimitive Quads $ do
					let dist = 0.3 :: GLfloat
					color (Color3 0.6 0.6 0.6 :: Color3 GLfloat)
					vertex (Vertex2 (-dist) (-dist))
					vertex (Vertex2 ( dist) (-dist))
					vertex (Vertex2 ( dist) ( dist))
					vertex (Vertex2 (-dist) ( dist))
					let shortDist = 0.1 :: GLfloat
					color (Color4 0.0 0.0 0.0 (1 - (fromIntegral energy / 10)) :: Color4 GLfloat)
					vertex (Vertex2 (-shortDist) (-shortDist))
					vertex (Vertex2 ( dist) (-shortDist))
					vertex (Vertex2 ( dist) ( shortDist))
					vertex (Vertex2 (-shortDist) ( shortDist))
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

			{-Sim.Mirror Sim.SW_NE -> do
				color (Color3 0.9 0.9 0.9 :: Color3 GLfloat)
				renderPrimitive Quads $ do
					let dist = 0.4 :: GLfloat
					vertex (Vertex2 (dist) (distFromCenter))
					vertex (Vertex2 (-distFromCenter) (-dist))
					vertex (Vertex2 (-dist) (-distFromCenter))
					vertex (Vertex2 (distFromCenter) (dist))-}
		  ) $ [(l,m) | (l,Just m) <- assocs worldMap]
	swapBuffers
	reportErrors


