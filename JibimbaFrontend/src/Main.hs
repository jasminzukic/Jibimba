-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Miso

import           Model
import           Update
import           View

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
 where
  initialAction = StartCounter  -- initial action to be executed on application load
  model         = initialModel  -- initial model
  update        = updateModel   -- update function
  view          = viewModel     -- view function
  events        = defaultEvents -- default delegated events
  subs          = []           -- empty subscription list -- every 1000000 Ticks
  mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')
