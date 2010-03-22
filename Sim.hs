
module Sim (simulate, World(..), Machine(..), Particle(..), ParticleType(..), Creature(..), Dir(..), MirrorDir(..), dirAngle, WorldHour(..){-hmm-}, hourFraction, dayLength) where

--import Data.Array
import Data.Array.Unboxed
import Data.List (genericLength, unzip4)
import Data.Maybe (isNothing)
import System.Random

-- .Diff
type WholeMap = {-Diff-}Array Loc
type SparseMultiMap a = [(Loc,a)]

type Loc = (Int,Int)
type Offset = (Int,Int) -- (not) aka dir
type WorldMap = WholeMap (Maybe Machine)
type WorldMovers = SparseMultiMap Particle
type WorldCreatures = SparseMultiMap Creature
type Pollution = Double
type WorldPollution = UArray Loc Pollution
newtype WorldHour = WorldHour Int
data World = World { worldMap :: WorldMap, worldParticles :: WorldMovers, worldCreatures :: WorldCreatures, worldPollution :: WorldPollution, worldHour :: WorldHour }
data Dir = North | East | South | West
	deriving (Enum,Bounded,Show)
data MirrorDir = NW_SE | SW_NE
	deriving (Enum,Show)
-- member names:
-- `m'nName where n is the initial of the constructor,
-- or `_' if it may be applicable to multiple constructors
data Machine
	= Generator { m_Dir :: Dir, m_Energy :: Int }
	| Mirror { mMDir :: MirrorDir, mMLeftSilvered, mMRightSilvered :: Bool }
	| Greenery {  }
	| Storm { mSEnergy :: Double, m_RNG :: StdGen }
	| Mountain {  }
	deriving (Show)
{-instance Show Machine where
	show (Generator{}) = "G"
	show (Mirror{}) = "M"-}
data Particle = Particle Dir ParticleType
data ParticleType
	= Energy Int  -- where Int strength > 0
	| Chaos StdGen
data Creature = Creature { creatureEnergy :: Double, creatureRNG :: StdGen } --energy system like Angband!.. only normally move every 10, but may communicate quicker
--don't separate it from the creature   data Brains = Brains { }--creatureMorale :: Double,

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
mirrorSilveredWhenGoingDirection machine dir = error ( "BUG! mirrorSilveredWhenGoingDirection should not be called with a non-mirror, namely " ++ show machine ++ " (with dir = " ++ show dir ++ ")" )

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
simMachine ::
	WorldMap -> Array Loc [Particle] -> Array Loc [Creature] -> WorldPollution
	-> Loc -> Maybe Machine
	-> (Maybe Machine, [Particle], [Creature], Pollution)
simMachine worldMap particleMap creatureMap pollutionMap loc maybeMachine =
  case maybeMachine of
    Nothing -> (Nothing, pHere, cHere, defaultNewPollution)
    (Just m) -> let
    			chaosRNGs = [rng | Particle _ (Chaos rng) <- pHere]
    			chaoslyRandomMachine :: StdGen -> Machine  -- could also return next g
			chaoslyRandomMachine rng = let (r,rng') = randomR (0,28) rng in
				if r < 4 then Generator (toEnum r) 5
				else if r < 16 then Mirror (toEnum (r `mod` 2)) (r<12) (r>=8)
				else if r < 20 then Storm (fromIntegral r - 16) rng'
				else if r < 25 then Greenery
				else Mountain  -- should chaos create/destroy MOUNTAINS???
    		in if not (null chaosRNGs) then (Just $ chaoslyRandomMachine $ head chaosRNGs, [], cHere, defaultNewPollution + 3)
	else
      case m of
	Generator dir energy -> if energy >= 5   -- pretty efficient generator: 80% efficiency
		then (Just $ m {m_Energy = energy - 5 + particleEnergyHere}, [Particle dir (Energy 4)], cHere, defaultNewPollution + 1)
		else (Just $ m {m_Energy = energy + 1 + particleEnergyHere }, [], cHere, defaultNewPollution)
	Mirror mdir _ _ -> (Just m, map (\p@(Particle pdir ptype) -> if mirrorSilveredWhenGoingDirection m pdir
					then Particle (mirror mdir pdir) (ptype)
					else p{- modifyingParticleDir $ mirror mdir-}) pHere, cHere, defaultNewPollution)
	Greenery -> (Just m, pHere, cHere, 0)  --a bit powerful pollution remover at the moment, but non-invasive, seems nice in practice
	Storm energy rng ->
				if newEnergy > today'sChaosParticleThreshold
					then (Just $ Storm 0 rng_storm, [newChaos], cHere, newPollution)
				else if newEnergy < 16
					then (Just $ Storm newEnergy rng_storm, [], cHere, newPollution)
				else (Nothing, [newChaos], cHere, newPollution + 3)
		where
			-- rng2', rng'', (-0.4,0.4), (5,17), (0,3)
			(rng_storm, rng2) = split rng
			(today'sChaosParticleThreshold, rng3) = randomR (5,17) rng2
			(rawPollutionEffect, rng4) = randomR (-0.4,0.4) rng3  --how should it affect energy???
			(newChaos'sDirection,rng5) = randomDir rng4
			newChaos'sRNG = rng5
			randomDir :: (RandomGen g) => g -> (Dir, g)
			randomDir rng_1 = let (dirN, rng_2) = randomR (0,3) rng_1 in (toEnum dirN, rng_2)
			newEnergy = energy + fromIntegral particleEnergyHere + pollutionEffect --eating pollution hurts the storm, spewing helps
			pollutionEffect = max (-defaultNewPollution) rawPollutionEffect
			newPollution = pollutionEffect + defaultNewPollution
			newChaos = Particle newChaos'sDirection (Chaos newChaos'sRNG)
	Mountain -> (Just m, [], cHere, 0)
  where
  	pHere = particleMap ! loc
	cHere = creatureMap ! loc
	particleEnergyHere = sum [e | Particle _ (Energy e) <- pHere]
	pollutionHere = pollutionMap ! loc  --should edges be dissipated off of? should there be any decrease in total? wind?! diagonals?
	makeRNG = mkStdGen $ (truncate(pollutionHere*1000000)) + (fst loc) + (100000*snd loc)
	defaultNewPollution = let
			neighborLocs = orthogonalNeighborLocsWithin (bounds pollutionMap) loc
			significantNeighborLocs = filter (\l -> case worldMap!l of Just Mountain -> False; _ -> True) neighborLocs
			neighborPollutions = map (pollutionMap !) significantNeighborLocs
			pollutionKept = pollutionHere * (1 - transferFraction*genericLength significantNeighborLocs)
			pollutionTaken = sum $ map (* transferFraction) neighborPollutions
			transferFraction = 1/16
		in pollutionKept + pollutionTaken

simMachine' ::
	WorldMap -> Array Loc [Particle] -> Array Loc [Creature] -> WorldPollution
	-> Loc -> Maybe Machine
	-> (Maybe Machine, [(Loc,Particle)], [(Loc,Creature)], Pollution)
simMachine' wm pm cm polluMap loc mm =
	let (res1,res2,res3,res4) = simMachine wm pm cm polluMap loc mm
	in (res1, map ((,) loc) res2, map ((,) loc) res3, res4)

--creatureMove :: WorldMap -> Array Loc [Particle] -> Array Loc [Creature] -> WorldPollution -> Loc -> [Creature] -> [(Loc,Creature)]
--creatureMove worldMap particleMap creatureMap pollutionMap loc creatures =
--	map (creatureMove' worldMap particleMap creatureMap pollutionMap loc) creatures

creatureMove :: WorldMap -> Array Loc [Particle] -> Array Loc [Creature] -> WorldPollution -> Loc -> Creature -> (Loc,Creature)
creatureMove worldMap particleMap creatureMap pollutionMap loc creature =
	let (loc', rng') = randomlyShiftLocLimitedly (\l -> inRange (bounds worldMap) l && isNothing (worldMap ! l)) loc (creatureRNG creature)
	in (loc', Creature { creatureEnergy = creatureEnergy creature, creatureRNG = rng'})

--hmm, may loop infinitely if can't even stay in the same place
randomlyShiftLocLimitedly :: RandomGen g => (Loc -> Bool) -> Loc -> g -> (Loc, g)
randomlyShiftLocLimitedly p loc rng =
	if p loc' then result else randomlyShiftLocLimitedly p loc rng'
	where result@(loc', rng') = randomlyShiftLoc loc rng

--rather arbitrary now, should it do diagonal movement? what if I want to use hexes sometime?
randomlyShiftLoc :: RandomGen g => Loc -> g -> (Loc, g)
randomlyShiftLoc loc@(x,y) rng =
	let
		(x', rng') = randomR (x-1, x+1) rng
		(y', rng'')= randomR (y-1, y+1) rng'
	in ((x',y'), rng'')

simulate :: World -> World
simulate (World oldMap oldParticles oldCreatures oldPollution oldHour) = let
	worldBounds = bounds oldMap
	particleArray = accumArray (flip (:)) [] worldBounds
		$ filter (inRange worldBounds . fst) $ map (particleMove) oldParticles
	oldCreatureArray = accumArray (flip (:)) [] worldBounds oldCreatures
	newCreatureArray = accumArray (flip (:)) [] worldBounds
		$ filter (inRange worldBounds . fst) $ map (uncurry (creatureMove oldMap particleArray oldCreatureArray oldPollution)) oldCreatures
	results = map (uncurry (simMachine' oldMap particleArray newCreatureArray oldPollution)) (assocs oldMap)--mapArrayWithIndices (simMachine w particleArray) w
	(machines,newParticle'ss,newCreature'ss,pollutions) = unzip4 results
	newParticles = concat newParticle'ss
	newCreatures = concat newCreature'ss
	newMap = listArray worldBounds machines
	newPollutions = listArray worldBounds pollutions  -- this assumes pollution and map have same bounds
        newHour = hourMove oldHour
	in World newMap newParticles newCreatures newPollutions newHour


-- | in range [0,1)
hourFraction :: WorldHour -> Rational
hourFraction (WorldHour n) = realToFrac n / realToFrac dayLength

hourMove :: WorldHour -> WorldHour
hourMove (WorldHour oldN) =
  let newN = oldN + 1 in
  if newN < dayLength
  then WorldHour newN
  else WorldHour (newN - dayLength)

dayLength :: Int
dayLength = 50

-- not as generic (in Array) as it could be
--zipArray :: Ix i => Array i e1 -> Array i e2 -> Array i (e1,e2)
--zipArray a1 a2

--mapArrayWithIndices :: (Loc -> a -> b) -> MapType Loc a -> MapType Loc b
--mapArrayWithIndices f a = array (bounds a) (map (uncurry f) (assocs a))

