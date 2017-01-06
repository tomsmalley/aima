{-# LANGUAGE MultiParamTypeClasses #-}

-- | Performance measuring environment simulator
module AIMA.Agent.Core where

import Data.List (genericLength)

-- | Agent (p, a) = (percept, action)
class (Show (agent p a)) => Agent agent p a where
    -- | Agent program that can have state
    agent :: agent p a          -- ^ Agent current state
          -> p                  -- ^ Current percept
          -> (a, agent p a)     -- ^ (Action, new Agent state)

-- | Environment (m, p, a) = (measure, percept, action)
class (Show m, Show (e m p a)) => TaskEnv e m p a where
    -- | Get current percept from the environment
    percept :: e m p a          -- ^ Environment
            -> p                -- ^ Current percept
    -- | Execute an action in the environment
    execute :: e m p a          -- ^ Environment
            -> a                -- ^ Action to perform
            -> e m p a          -- ^ Resultant environment
    -- | Get the measure of the environment
    measure :: e m p a          -- ^ Environment
            -> m                -- ^ Measure (total)

-- | Single step simulation for an environment and agent
step :: (TaskEnv e m p a, Agent agent p a)
     => (e m p a, agent p a)    -- ^ Initial state (environment, agent)
     -> (e m p a, agent p a)    -- ^ Resultant state (environment, agent)
step (e, a) = (execute e action, a')
    where (action, a') = agent a (percept e)

-- | Step simulation n times for an environment and agent
stepN :: (TaskEnv e m p a, Agent agent p a)
      => Int                    -- ^ Number of steps to execute
      -> agent p a              -- ^ Agent
      -> e m p a                -- ^ Initial environment
      -> (e m p a, agent p a)   -- ^ Resultant state (environment, agent)
stepN n agent env = iterate step (env, agent) !! n

-- | Test an agent in several environments, output to stdout
test :: (Real m, TaskEnv e m p a, Agent agent p a)
     => Int                     -- ^ Number of steps to execute
     -> agent p a               -- ^ The agent to use
     -> [e m p a]               -- ^ The initial environments to use
     -> IO ()                   -- ^ Output printed to console
test n a es = do
    putStrLn . unlines . map pp $ zip es esa'
    putStrLn $ "Mean performance measure: " ++ show avg
    where esa' = stepN n a <$> es -- List of (final env, final agent)
          avg = mean $ map (measure . fst) esa'
          pp (e, (e', a')) = unlines [ "Initial Environment: " ++ show e
                                     , "Final Environment: " ++ show e'
                                     , "Final Agent: " ++ show a'
                                     ]

-- | Helper function for calculating the mean
mean :: (Real a, Fractional b) => [a] -> b
mean as = realToFrac (sum as) / genericLength as
