{-# LANGUAGE OverloadedStrings #-}

module Model where

import           Miso.String                    ( MisoString )

-- | Type synonym for an application model
data Model = Model
  { state :: States
  , inputField :: MisoString
  , numberField :: MisoString
  , timerField :: MisoString
  , invalidTeamName :: Bool
  , invalidSyntagma :: Bool
  , buttonEnabled :: Bool
  , generator :: Generator
  , syntagmas :: [(String, Int)] -- Syntagmas and how much time it took for them to be guessed
  , guessed :: [(String, Int)]
  , teams :: [(String, Int)]
  , currentTeam :: Int
  , timer :: Int
  , time :: Int
  , timeElapsed :: Int
  , runda :: Int
  } deriving (Eq, Show)

data Generator = Idle | Generating | Done deriving (Eq, Show)

data States = MainMenu
            | BasicRules
            | AliasRules
            | CharadesRules
            | OneWordRules
            | Settings
            | TeamInput
            | SynInput
            | RoundPrep
            | Gameplay
            | TimeOut
            | GameOver
            deriving (Eq, Show)

-- | Sum type for application events
data Action
  = ToBasicRules
  | ToAliasRules
  | ToCharadesRules
  | ToOneWordRules
  | ToSettings
  | ToTeamInput
  | ToSynInput
  | ToRoundPrep
  | ToGameplay
  | ToGameOver
  | ToMainMenu
  | BackToMainMenu
  | SettingsToMainMenu

  | UpdateTeamField   MisoString
  | UpdateSynField    MisoString
  | UpdateNumberField MisoString
  | UpdateTimerField  MisoString

  | NextWord
  | SkipWord

  | AddTeam
  | AddSyn
  | GenerateSyntagmas
  | PostSyntagmas
  | ShuffleTeams [(String, Int)]
  | ShuffleSyns [(String, Int)]
  | SetTime
  | StartCounter
  | Tick
  | NoOp
  deriving (Show, Eq)


initialModel :: Model
initialModel = Model
  { state           = MainMenu
  , inputField      = mempty
  , numberField     = "25"
  , timerField      = "60"
  , invalidTeamName = False
  , invalidSyntagma = False
  , buttonEnabled   = False
  , generator       = Idle
  , syntagmas       = []
  , guessed         = []
  , teams           = []
  , currentTeam     = 0
  , timer           = 60
  , time            = -1
  , timeElapsed     = 0
  , runda           = 1
  }
