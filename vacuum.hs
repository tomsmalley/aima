{-# LANGUAGE MultiParamTypeClasses #-}

module Vacuum where

import Prelude hiding (Left, Right)
import Control.Monad.Trans.State
import Control.Monad (replicateM)
import Data.List (genericLength)
import qualified Data.Map as M
import Data.Map (Map)


----------------------
-- Environment
----------------------

class (Show (agent p a)) => Agent agent p a where
    agent :: p -> agent p a -> (a, agent p a)

-- | Environment
class (Show m, Show (e m p a)) => TaskEnv e m p a where
    -- Get current percept
    percept :: e m p a -> p
    -- Execute an action
    execute :: e m p a -> a -> e m p a
    -- Get the measure
    measure :: e m p a -> m

step :: (TaskEnv e m p a, Agent agent p a)
     => (e m p a, agent p a)
     -> (e m p a, agent p a)
step (e, a) = (execute e action, a')
    where (action, a') = agent (percept e) a

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

sumOne :: (TaskEnv e m p a, Agent agent p a)
       => e m p a -> agent p a -> String
sumOne env agent = show agent ++ " in " ++ show env
                ++ " scored " ++ show m
    where m = measure . fst $ iterate step (env, agent) !! 1000

test :: (TaskEnv e m p a, Agent agent p a)
     => [e m p a] -> agent p a -> IO ()
test es a = putStrLn $ unlines [ sumOne e a | e <- es ]

-- | Initial state generator
initials :: [VacuumWorld Int Percept Action]
initials = [ VW m p 0 | m <- map toMap states, p <- [A, B] ]
    where states = [ (s1, s2) | s1 <- [Clean, Dirty]
                              , s2 <- [Clean, Dirty] ]
          toMap (s1, s2) = M.fromList [ (A, s1), (B, s2) ]

-- | Helper function for calculating the mean
mean :: (Real a, Fractional b) => [a] -> b
mean as = realToFrac (sum as) / genericLength as


---------------------------
-- Agent implementations --
---------------------------

-------------------------
-- Simple reflex agent --
-------------------------

data ReflexVacuumAgent p a = RVA deriving (Show)
instance Agent ReflexVacuumAgent Percept Action where
    agent (Percept _ (Just Dirty)) a = (Suck, a)
    agent (Percept (Just A) _) a = (Right, a)
    agent (Percept (Just B) _) a = (Left, a)
    agent _ a = (NoOp, a)

-----------------------
-- Model based agent --
-----------------------

-- | Internal model of environment
data Model = Model { mMap :: Map Position Status } deriving (Eq)
instance Show Model where
    show (Model ps) = M.foldMapWithKey f ps
      where f k v = "[ " ++ show k ++ " " ++ show v ++ " ] "

-- | Initial model of world (assume it is all dirty!)
agentInitialState :: ModelVacuumAgent Percept Action
agentInitialState = MVA $ Model (M.fromList [ (A, Dirty), (B, Dirty) ])

data ModelVacuumAgent p a = MVA { mvaModel :: Model } deriving (Eq)
instance Show (ModelVacuumAgent p a) where
    show (MVA m) = "Model: " ++ show m
instance Agent ModelVacuumAgent Percept Action where
    agent p a = (action, MVA model)
        where (action, model) = modelVacuumAgent p (mvaModel a)

-- | Agent program
modelVacuumAgent :: Percept -> Model -> (Action, Model)
modelVacuumAgent (Percept (Just p) (Just s)) m
  | s == Dirty = (Suck, m')
  | otherwise  = case M.lookup (sflip p) (mMap m) of
                      Just Dirty -> (moveFrom p, m')
                      _ -> (NoOp, m')
  where sflip A = B
        sflip B = A
        moveFrom A = Right
        moveFrom B = Left
        m' = Model $ M.insert p Clean (mMap m)
