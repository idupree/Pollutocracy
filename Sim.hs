
module Sim (simulate, World(..), Machine(..), Particle(..), ParticleType(..), Creature(..), Dir(..), MirrorDir(..), dirAngle, WorldHour(..){-hmm-}, hourFraction, dayLength) where

-- Generators emit pollution, as well as bolts of energy that
-- bounce off mirrors and can energize other generators.
--
-- Mountains get in the way.
--
-- Trees absorb pollution.
--
-- Chaos-storms emit chaos, which changes these "machines" randomly
-- into other types of "machines", and water, which for some odd
-- reason is attracted to pollution yet destroyed by bolts of energy.
--
-- (The reason is obviously that I wanted to see what it would be like
-- if I had something be affected by pollution in some nontrivial way.
-- Or that I wanted flowing rivers that go in a direction!)

-- The guy wanders around mostly randomly and usually dies pretty soon.
-- Pay him/her no mind. (constructor "Creature")

import Data.Array.Unboxed
import Data.List (genericLength, unzip4, maximumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust, isNothing)
import System.Random

-- (TODO: Maybe use DiffArray or some modern data structure?)
type WholeMap = Array Loc
type SparseMultiMap a = [(Loc,a)]

type Loc = (Int,Int)
type Offset = (Int,Int) -- (not) aka dir
type WorldMap = WholeMap (Maybe Machine)
type WorldMovers = SparseMultiMap Particle
type WorldCreatures = SparseMultiMap Creature
type Pollution = Double
type WorldPollution = UArray Loc Pollution
newtype WorldHour = WorldHour Int
data World = World
	{ worldMap :: WorldMap
	, worldParticles :: WorldMovers
	, worldCreatures :: WorldCreatures
	, worldPollution :: WorldPollution
	, worldHour :: WorldHour
	}
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
	| Riverbed { mRDepth :: Int }
	deriving (Show)
{-instance Show Machine where
	show (Generator{}) = "G"
	show (Mirror{}) = "M"-}
data Particle = Particle Dir ParticleType
data ParticleType
	= Energy Int  -- where Int strength > 0
	| Chaos StdGen
data Creature
	-- Energy system like Angband; only normally move every 10 energy,
	-- but may communicate quicker.
	= Creature { creatureEnergy :: Double, creatureRNG :: StdGen }
	| Water { creatureRNG :: StdGen }

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
mirrorSilveredWhenGoingDirection machine dir =
	error ( "BUG! mirrorSilveredWhenGoingDirection should not be" ++
		" called with a non-mirror, namely " ++ show machine ++
		" (with dir = " ++ show dir ++ ")" )

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
simMachine terrainMap particleMap creatureMap pollutionMap loc maybeMachine =
  case maybeMachine of
    Nothing -> (Nothing, pHere, cHere, defaultNewPollution)
    (Just m) ->
	-- If a chaos particle hits a machine, that mutates the machine
	-- rather than the machine here doing anything this turn.
	let
		chaosRNGs = [rng | Particle _ (Chaos rng) <- pHere]
		chaoslyRandomMachine :: StdGen -> Machine
					-- could also return next g
		chaoslyRandomMachine rng = let (r,rng') = randomR (0,28) rng in
			if r < 4 then Generator (toEnum r) 5
			else if r < 16 then Mirror (toEnum (r `mod` 2)) (r<12) (r>=8)
			else if r < 20 then Storm (fromIntegral r - 16) rng'
			else if r < 25 then Greenery
			else Mountain  -- should chaos create/destroy MOUNTAINS???
	in if not (null chaosRNGs) then
		(Just $ chaoslyRandomMachine $ head chaosRNGs,
		 [], cHere, defaultNewPollution + 3)
	else
      case m of
	-- Generators generate energy over time (and when an energy particle
	-- hits them), and every time they have enough energy stored,
	-- they emit an energy particle using that energy.
	-- It's a pretty efficient generator: 80% efficiency.
	Generator dir energy -> if energy >= 5
		then (Just $ m {m_Energy = energy - 5 + particleEnergyHere},
		      [Particle dir (Energy 4)], cHere, defaultNewPollution + 1)
		else (Just $ m {m_Energy = energy + 1 + particleEnergyHere },
		      [], cHere, defaultNewPollution)
	Mirror mdir _ _ -> (Just m,
			    map (\p@(Particle pdir ptype) ->
					if mirrorSilveredWhenGoingDirection m pdir
					then Particle (mirror mdir pdir) (ptype)
					else p)
				pHere,
			    cHere,
			    defaultNewPollution)
	-- Greenery absorbs pollution by letting pollution diffuse into it
	-- and then remaining unpolluted.
	-- It's a bit powerful pollution remover at the moment,
	-- but non-invasive; seems nice in practice.
	Greenery -> (Just m, pHere, cHere, 0)
	-- Storms are strange.
	-- They randomly absorb or emit pollution.
	-- Absorbing pollution decreases their energy,
	--   and emitting increases their energy.
	-- Storms emit "chaos", which travels in a direction
	-- until it hits something and mutates it, and "water",
	-- which is a Creature that follows pollution.  These
	-- emissions depend on the storm's energy and on chance.
	Storm energy rng ->
		if newEnergy > today'sChaosParticleThreshold
		then (Just $ Storm 0 rng_storm,
		      [newChaos], newWater:cHere, newPollution)
		else if newEnergy < 16
		then (Just $ Storm newEnergy rng_storm,
		      [], newWater:cHere, newPollution)
		else (Nothing,
		      [newChaos], cHere, newPollution + 3)
	    where
		(rng_storm, rng2) = split rng
		(today'sChaosParticleThreshold, rng3) = randomR (5,17) rng2
		(rawPollutionEffect, rng4) = randomR (-0.4,0.4) rng3
		(newChaos'sDirection,rng5) = randomDir rng4
		newChaos'sRNG = rng5
		randomDir :: (RandomGen g) => g -> (Dir, g)
		randomDir rng_1 = let (dirN, rng_2) = randomR (0,3) rng_1 in (toEnum dirN, rng_2)
		newEnergy = energy + fromIntegral particleEnergyHere + pollutionEffect
		pollutionEffect = max (-defaultNewPollution) rawPollutionEffect
		newPollution = pollutionEffect + defaultNewPollution
		newChaos = Particle newChaos'sDirection (Chaos newChaos'sRNG)
		newWater = (Water newChaos'sRNG)--hack: sharing RNG?
	-- Mountains just sit there and get in the way of everything.
	-- (Perhaps they could occasionally produce rain?)
	Mountain -> (Just m, [], cHere, 0)
	-- (Riverbeds are an unused constructor currently.)
	Riverbed {} -> (Just m, [], cHere, 0)
  where
	pHere = particleMap ! loc
	cHere = creatureMap ! loc
	particleEnergyHere = sum [e | Particle _ (Energy e) <- pHere]
	-- Pollution diffusion:
	-- should edges of the map be dissipated off of?
	-- should there be any decrease in total?
	-- wind?! diagonals?
	pollutionHere = pollutionMap ! loc
	defaultNewPollution = let
		neighborLocs = orthogonalNeighborLocsWithin (bounds pollutionMap) loc
		significantNeighborLocs = filter (\l -> case terrainMap!l of
			Just Mountain -> False; _ -> True) neighborLocs
		neighborPollutions = map (pollutionMap !) significantNeighborLocs
		pollutionKept = pollutionHere *
			(1 - transferFraction*genericLength significantNeighborLocs)
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

creatureMove :: WorldMap -> Array Loc [Particle] -> Array Loc [Creature] -> WorldPollution -> Loc -> Creature -> (Loc,Creature)
creatureMove machineMap particleMap creatureMap pollutionMap loc creature =
	let
		(rng_pollutionEntropy, rng_creature) = split (creatureRNG creature)
		movetoAble l = inRange (bounds machineMap) l &&
			case machineMap ! l of
				Nothing -> True
				Just (Generator{}) -> True
				_ -> False
		choices = filter movetoAble $
			orthogonalNeighborLocsWithin (bounds machineMap) loc
		rawDraws = map (\l -> (pollutionMap ! l, l)) choices
		pollutionTweaks = randomRs (0, 0.1) rng_pollutionEntropy
		perceivedDraws = zipWith (\ (p,l) t -> (p+t, l)) rawDraws pollutionTweaks
		--tie? arbitrary winner! more pollution is better for water!
		best = maximumBy (comparing fst) perceivedDraws
		loc' = snd best
		newCreature = (loc', creature {creatureRNG = rng_creature})
		--water is killed by all particles!!!!(so is GUY.)
		die = null choices ||
			not (null (particleMap ! loc)) ||
			not (null (particleMap ! loc')) ||
			isJust (machineMap ! loc')
		-- HACK method to delete the creature:
		nobody = ((-100000,-100000),
			  creature {creatureRNG = rng_creature})
	in if die then nobody else newCreature


simulate :: World -> World
simulate (World oldMap oldParticles oldCreatures oldPollution oldHour) = let
	worldBounds = bounds oldMap
	particleArray = accumArray (flip (:)) [] worldBounds
		$ filter (inRange worldBounds . fst)
		$ map (particleMove) oldParticles
	oldCreatureArray = accumArray (flip (:)) [] worldBounds oldCreatures
	newCreatureArray = accumArray (flip (:)) [] worldBounds
		$ filter (inRange worldBounds . fst)
		$ map (uncurry (creatureMove oldMap particleArray
				oldCreatureArray oldPollution)) oldCreatures
	results = map (uncurry (simMachine' oldMap particleArray
				newCreatureArray oldPollution)) (assocs oldMap)
			--mapArrayWithIndices (simMachine w particleArray) w
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

