
module Sim (simulate, World(..), Machine(..), Particle(..), ParticleType(..), Dir(..), MirrorDir(..), dirAngle) where

--import Data.Array
import Data.Array.Unboxed
import Data.List (genericLength)

-- .Diff
type MapType = {-Diff-}Array

type Loc = (Int,Int)
type Offset = (Int,Int) -- (not) aka dir
type WorldMap = MapType Loc (Maybe Machine)
type WorldMovers = [(Loc,Particle)]
type Pollution = Double
type WorldPollution = UArray Loc Pollution
data World = World { worldMap :: WorldMap, worldParticles :: WorldMovers, worldPollution :: WorldPollution }
data Dir = North | East | South | West
	deriving (Enum,Bounded)
data MirrorDir = NW_SE | SW_NE
	deriving (Enum)
-- member names:
-- `m'nName where n is the initial of the constructor,
-- or `_' if it may be applicable to multiple constructors
data Machine
	= Generator { m_Dir :: Dir, m_Energy :: Int }
	| Mirror { mMDir :: MirrorDir, mMLeftSilvered, mMRightSilvered :: Bool }
{-instance Show Machine where
	show (Generator{}) = "G"
	show (Mirror{}) = "M"-}
data Particle = Particle Dir ParticleType
data ParticleType = Energy Int  -- where Int strength > 0
-- OpenGL uses degrees, and that's what this is used for, so use degrees.
dirAngle :: Num{-Floating-} a => Dir -> a
dirAngle East = 0 --0
dirAngle North = 90 --pi * 0.5
dirAngle West = 180 --pi
dirAngle South = 270 --pi * 1.5
dirOffset :: Dir -> Offset
dirOffset North = (0,1)
dirOffset South = (0,-1)
dirOffset East = (1,0)
dirOffset West = (-1,0)
shiftByOffset :: Offset -> Loc -> Loc
shiftByOffset (dx,dy) (x,y) = (x+dx,y+dy)
shift :: Dir -> Loc -> Loc
shift = shiftByOffset . dirOffset
particleMove :: (Loc,Particle) -> (Loc,Particle)
particleMove (loc,p@(Particle dir _)) = (shift dir loc,p)
--particleMove ((x,y),p@(Particle dir _)) = ((x+dx,y+dy),p)
	--where (dx,dy) = dirOffset dir
	{-where newLoc = case dir of  --careful, directions have real meaning here
		North -> (x,y+1)
		South -> (x,y-1)
		East -> (x+1,y)
		West -> (x-1,y)-}
modifyingParticleDir :: (Dir -> Dir) -> (Particle -> Particle)
modifyingParticleDir f (Particle d t) = Particle (f d) t
mirrorSilveredWhenGoingDirection :: Machine{-Mirror-} -> Dir -> Bool
mirrorSilveredWhenGoingDirection mir@(Mirror {}) dir =
	(case mMDir mir of
		NW_SE -> case dir of
			North -> mMLeftSilvered
			East -> mMLeftSilvered
			South -> mMRightSilvered
			West -> mMRightSilvered
		SW_NE -> case dir of
			South -> mMLeftSilvered
			East -> mMLeftSilvered
			North -> mMRightSilvered
			West -> mMRightSilvered
	) mir
mirror :: MirrorDir -> Dir -> Dir
mirror NW_SE North = West
mirror NW_SE West  = North
mirror NW_SE South = East
mirror NW_SE East  = South
mirror SW_NE North = East
mirror SW_NE East  = North
mirror SW_NE South = West
mirror SW_NE West  = South
orthogonalNeighborLocsWithin :: (Loc,Loc) -> Loc -> [Loc]
orthogonalNeighborLocsWithin bound center =
	filter (inRange bound) $ map (flip shift center) [North,East,South,West]
	
-- don't yet use a monad; see how this goes
-- hmm, randomness can wait too
simMachine :: WorldMap -> Array Loc [Particle] -> WorldPollution -> Loc -> Maybe Machine -> (Maybe Machine, [Particle], Pollution)
simMachine _wm pm pollutionMap loc maybeMachine =
  case maybeMachine of
    Nothing -> (Nothing, pHere, defaultNewPollution)
    (Just m) ->
      case m of
	Generator dir energy -> if energy >= 5   -- pretty efficient generator: 80% efficiency
		then (Just $ m {m_Energy = energy - 5}, [Particle dir (Energy 4)], defaultNewPollution + 1)
		else (Just $ m {m_Energy = m_Energy m + 1 + sum [e | Particle _ (Energy e) <- pHere] }, [], defaultNewPollution)
	Mirror mdir _ _ -> (Just m, map (\p@(Particle pdir ptype) -> if mirrorSilveredWhenGoingDirection m pdir
					then Particle (mirror mdir pdir) (ptype)
					else p{- modifyingParticleDir $ mirror mdir-}) pHere, defaultNewPollution)
  where
  	pHere = pm ! loc
	pollutionHere = pollutionMap ! loc  --should edges be dissipated off of? should there be any decrease in total? wind?!
	defaultNewPollution = let
			neighborLocs = orthogonalNeighborLocsWithin (bounds pollutionMap) loc
			neighborPollutions = map (pollutionMap !) neighborLocs
			pollutionKept = pollutionHere * (1 - transferFraction*genericLength neighborLocs)
			pollutionTaken = sum $ map (* transferFraction) neighborPollutions
			transferFraction = 1/16
		in pollutionKept + pollutionTaken

simMachine' :: WorldMap -> Array Loc [Particle] -> WorldPollution -> Loc -> Maybe Machine -> (Maybe Machine, [(Loc,Particle)], Pollution)
simMachine' wm pm polluMap loc mm = let (res1,res2,res3) = simMachine wm pm polluMap loc mm in (res1, map ((,) loc) res2, res3)

simulate :: World -> World
simulate (World oldMap oldParticles oldPollution) = let
	worldBounds = bounds oldMap
	particleArray = accumArray (flip (:)) [] worldBounds
		$ filter (inRange worldBounds . fst) $ map (particleMove) oldParticles
	results = map (uncurry (simMachine' oldMap particleArray oldPollution)) (assocs oldMap)--mapArrayWithIndices (simMachine w particleArray) w
	(machines,newParticle'ss,pollutions) = unzip3 results
	newParticles = concat newParticle'ss
	newMap = listArray worldBounds machines
	newPollutions = listArray worldBounds pollutions  -- this assumes pollution and map have same bounds
	in World newMap newParticles newPollutions

-- not as generic (in Array) as it could be
--zipArray :: Ix i => Array i e1 -> Array i e2 -> Array i (e1,e2)
--zipArray a1 a2

--mapArrayWithIndices :: (Loc -> a -> b) -> MapType Loc a -> MapType Loc b
--mapArrayWithIndices f a = array (bounds a) (map (uncurry f) (assocs a))

