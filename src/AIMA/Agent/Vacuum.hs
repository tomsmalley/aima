{-# LANGUAGE MultiParamTypeClasses #-}

-- | Vacuum world implementation, with agents
module AIMA.Agent.Vacuum where

import Prelude hiding (Left, Right)
import qualified Data.Map as M
import Data.Map (Map)
import AIMA.Agent.Core

--------------------------------
-- VacuumWorld implementation --
--------------------------------

-- Possible positions
data Position = A | B deriving (Eq, Ord, Show)
-- Possible status for positions
data Status = Clean | Dirty deriving (Eq, Show)
-- Possible actions
data Action = Left | Suck | Right | NoOp deriving (Eq, Show)
-- Data contained in a percept
data Percept = Percept (Maybe Position) (Maybe Status) deriving (Eq, Show)

-- VacuumWorld is just positions and statuses, with vacuum position
data VacuumWorld m p a = VW { vwMap :: Map Position Status
                            , vwPos :: Position
                            , vwMeasure :: m
                            } deriving (Eq)

instance TaskEnv VacuumWorld Int Percept Action where
    percept (VW ps p _) = Percept (Just p) (M.lookup p ps)
    execute (VW ps p m) a
        | a == Left  = VW ps A (m' - 1) -- Move left, reduce score
        | a == Right = VW ps B (m' - 1) -- Move right, reduce score
        | a == Suck  = VW (M.insert p Clean ps) p m' -- Just clean position
        | otherwise  = VW ps p m' -- Just update score
        where m' = m + sum (f <$> ps) -- Old score plus new score
              f Clean = 1
              f Dirty = 0
    measure = vwMeasure

instance (Show m) => Show (VacuumWorld m p a) where
    show (VW ps p m) = M.foldMapWithKey f ps ++ "score: " ++ show m
      where f k v = "[ " ++ show k ++ " " ++ show v ++ noot k ++ " ] "
            noot x = if x == p then " noot" else ""

-- | Initial state generator
initialWorld :: [VacuumWorld Int Percept Action]
initialWorld = [ VW m p 0 | m <- map toMap states, p <- [A, B] ]
    where states = [ (s1, s2) | s1 <- [Clean, Dirty]
                              , s2 <- [Clean, Dirty] ]
          toMap (s1, s2) = M.fromList [ (A, s1), (B, s2) ]

---------------------------
-- Agent implementations --
---------------------------

-- | Simple reflex agent
data ReflexVacuumAgent p a = RVA deriving (Eq)

instance Show (ReflexVacuumAgent p a) where
    show _ = "ReflexAgent"

instance Agent ReflexVacuumAgent Percept Action where
    agent a (Percept _ (Just Dirty)) = (Suck, a)
    agent a (Percept (Just A) _) = (Right, a)
    agent a (Percept (Just B) _) = (Left, a)
    agent a _ = (NoOp, a)


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
