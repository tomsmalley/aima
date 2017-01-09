{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}

-- | 2D vacuum world implementation, with agents
module AIMA.Agent.Vacuum2D where

import Prelude hiding (Left, Right)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (groupBy)
import AIMA.Agent.Core
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

--------------------------------
-- VacuumWorld implementation --
--------------------------------

-- Possible status for positions
data Status = Clean | Dirty deriving (Eq, Show)
-- Possible actions
data Action = Up | Down | Left | Right | Suck | NoOp
    deriving (Eq, Show, Bounded, Enum)

-- Data contained in a percept
data Percept = Percept (Maybe Position) (Maybe Status) deriving (Eq, Show)

data Position = P { _x :: Int
                  , _y :: Int
                  } deriving (Eq)
instance Show Position where
    show (P x y) = show x ++ "|" ++ show y
instance Ord Position where
    compare (P x y) (P x' y') = case y `compare` y' of
                                  EQ -> x `compare` x'
                                  c  -> c

makeLenses ''Position

-- | Changes position based on action. Need to check if resultant position is
-- out of the bounds of the environment.
updatePosition :: Action -> Position -> Position
updatePosition Up = over y (+ 1)
updatePosition Down = over y (subtract 1)
updatePosition Left = over x (subtract 1)
updatePosition Right = over x (+ 1)
updatePosition _ = id

-- VacuumWorld is just positions and statuses, with vacuum position
data VacuumWorld2D m p a = VW2 { vwMap :: Map Position Status
                               , vwPos :: Position
                               , vwMeasure :: m
                               } deriving (Eq)

instance TaskEnv VacuumWorld2D Int Percept Action where
    percept (VW2 ps p _) = Percept (Just p) (M.lookup p ps)
    execute (VW2 ps p m) a = case a of
        Suck -> VW2 (M.insert p Clean ps) p m' -- Just clean position
        NoOp -> VW2 ps p m' -- Just update score
        _    -> VW2 ps p' (m' - 1) -- Move and update score
        where m' = m + sum (f <$> ps) -- Old score plus new score
              f Clean = 1
              f Dirty = 0
              p' = if M.member (updatePosition a p) ps
                      then (updatePosition a p)
                      else p
    measure = vwMeasure

instance (Show m) => Show (VacuumWorld2D m p a) where
    show (VW2 ps p m) = unlines . map (show . map f) . groupBy g $ M.toAscList ps
        where g (pos, _) (pos', _) = _y pos == _y pos'
              f (p', s) = show s ++ if p' == p then " noot" else ""

-- | Initial state generator
initialWorlds :: [VacuumWorld2D Int Percept Action]
initialWorlds = [ squareWorld ]

squareWorld :: VacuumWorld2D Int Percept Action
squareWorld = VW2 (M.fromList $ map (\x -> (x, Dirty)) positions) (P 0 0) 0
    where positions = [ P x y | x <- [0..4], y <- [0..4] ]

plusWorld :: VacuumWorld2D Int Percept Action
plusWorld = VW2 (M.fromList $ map (\x -> (x, Dirty)) positions) (P 0 0) 0
    where positions = [                     P 0 1
                      , P (-2) 0, P (-1) 0, P 0 0, P 1 0, P 2 0
                      ,                     P 0 (-1)
                      ]

---------------------------
-- Agent implementations --
---------------------------

-- | Simple reflex agent
data ReflexVacuumAgent p a = RVA deriving (Eq)

instance Show (ReflexVacuumAgent p a) where
    show _ = "ReflexAgent"

-- We can't reason about the position since we don't know the environment
instance Agent ReflexVacuumAgent Percept Action where
    agent a (Percept _ (Just Dirty)) = (Suck, a)
    agent a (Percept _ _) = (Right, a)


-- | Randomised agent
data RandomVacuumAgent p a = RandomVA StdGen

instance Show (RandomVacuumAgent p a) where
    show _ = "RandomAgent"

instance Agent RandomVacuumAgent Percept Action where
    agent (RandomVA g) _ = (toEnum a, RandomVA g')
        where (a, g') = randomR ( fromEnum (minBound :: Action)
                                , fromEnum (maxBound :: Action)) g

initialRandomAgent :: RandomVacuumAgent Percept Action
initialRandomAgent = RandomVA $ mkStdGen 0

{-

-- | Vacuum world agent which holds a model of the world
data ModelVacuumAgent p a = MVA { mvaModel :: Model } deriving (Eq)

instance Show (ModelVacuumAgent p a) where
    show (MVA m) = "ModelAgent: " ++ show m

instance Agent ModelVacuumAgent Percept Action where
    agent a (Percept (Just p) (Just s))
        | s == Dirty = (Suck, a')
        | otherwise  = case M.lookup (sflip p) (mMap $ mvaModel a) of
                            Just Dirty -> (moveFrom p, a')
                            _ -> (NoOp, a')
        where sflip A = B
              sflip B = A
              moveFrom A = Right
              moveFrom B = Left
              a' = MVA . Model $ M.insert p Clean (mMap $ mvaModel a)
    agent a (Percept _ _) = (NoOp, a)

-- | Internal model of environment
data Model = Model { mMap :: Map Position Status } deriving (Eq)

instance Show Model where
    show (Model ps) = M.foldMapWithKey f ps
      where f k v = "[ " ++ show k ++ " " ++ show v ++ " ] "

-- | Initial model of world (assume it is all dirty!)
initialModelAgent :: ModelVacuumAgent Percept Action
initialModelAgent = MVA $ Model (M.fromList [ (A, Dirty), (B, Dirty) ])
-}
