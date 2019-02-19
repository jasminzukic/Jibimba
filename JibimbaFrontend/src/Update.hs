-- | Haskell language pragma
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Update
  ( updateModel
  )
where

-- | Miso framework import
import           Miso                    hiding ( on )
import           Miso.String                    ( fromMisoString
                                                , ms
                                                )

import           Model

import           Data.Aeson                     ( eitherDecodeStrict )
import           Data.Char                      ( isLetter )
import           Data.List                      ( delete )
import           Control.Monad                  ( forM_ )
import           Control.Concurrent             ( threadDelay )
import           System.Random                  ( randomRIO )
import           System.Random.Shuffle          ( shuffleM )
import           JavaScript.Web.XMLHttpRequest



-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

-- UPDATE state variable
updateModel ToTeamInput g =
  noEff g { state = TeamInput, invalidTeamName = False, generator = Idle }

updateModel ToBasicRules    g = noEff g { state = BasicRules }
updateModel ToAliasRules    g = noEff g { state = AliasRules }
updateModel ToCharadesRules g = noEff g { state = CharadesRules }
updateModel ToOneWordRules  g = noEff g { state = OneWordRules }

updateModel ToSettings      g = noEff g { state = Settings }
updateModel ToSynInput g@Model {..} =
  g { state = if length teams >= 2 then SynInput else TeamInput }
    <# pure AddTeam

updateModel ToRoundPrep g@Model {..} =
  g { state     = if not (null syntagmas) then RoundPrep else SynInput
    , generator = Idle
    }
    <# do
         shuff <- shuffleM teams
         return $ ShuffleTeams shuff


updateModel ToGameplay g@Model {..} =
  g { state = Gameplay, time = timer, timeElapsed = 0 } <# do
    shuff <- shuffleM syntagmas
    return $ ShuffleSyns shuff

updateModel PostSyntagmas g@Model {..} = g <# do
  postSyntagmas syntagmas
  return ToMainMenu

updateModel ToMainMenu g = -- noEff initialModel umjesto ovoga dolje.
                           noEff g {                 -- ne treba ako ću dodati settingse
                                     state           = MainMenu
                                   , runda           = 1
                                   , syntagmas       = []
                                   , teams           = []
                                   , guessed         = []
                                   , currentTeam     = 0
                                   , invalidSyntagma = False
                                   , invalidTeamName = False
                                   , inputField      = mempty
                                   , numberField     = ms $ show 25
                                   }

updateModel BackToMainMenu     g            = noEff g { state = MainMenu }

updateModel SettingsToMainMenu g@Model {..} = noEff g { state = MainMenu
                                                      , timer = t
                                                      }
 where
  t' = fromMisoString timerField
  t | t' < 10   = 10
    | t' > 120  = 120
    | otherwise = t'


-- UPDATE nonstate variables

updateModel (ShuffleTeams t) g@Model {..} = noEff g { teams = t }
updateModel (ShuffleSyns s) g@Model {..} =
  noEff g { syntagmas = s, generator = Done }

updateModel NextWord g@Model {..} = if length syntagmas <= 1
  then noEff g { syntagmas   = newGuessed
               , guessed     = []
               , teams       = updatedTeams
               , currentTeam = (currentTeam + 1) `mod` length teams
               , time        = 0
               , runda       = runda + 1
               , state       = if runda >= 3 then GameOver else RoundPrep
               }
  else noEff g { syntagmas     = newSyns
               , guessed       = newGuessed
               , teams         = updatedTeams
               , timeElapsed   = 0
               , buttonEnabled = True
               }
 where
  newSyns      = tail syntagmas
  newGuessed   = (fst syn, snd syn + timeElapsed) : guessed
  syn          = head syntagmas
  updatedTeams = updateTeams teams currentTeam 1

updateModel SkipWord g@Model {..} = if length syntagmas == 1
  then noEff g
  else noEff g { syntagmas = tail syntagmas ++ [head syntagmas]
               , teams     = updateTeams teams currentTeam (-1)
               }

updateModel AddTeam g@Model {..} = noEff g
  { teams = if isValidTeam team teams then (team, 0) : teams else teams
  , inputField = if isValidTeam team teams then mempty else inputField
  , invalidTeamName = not $ isValidTeam team teams
  }
  where team = fromMisoString inputField

updateModel AddSyn g@Model {..} = noEff g
  { syntagmas = if isValidSyntagma syn then (syn, 0) : syntagmas else syntagmas
  , inputField = if isValidSyntagma syn then mempty else inputField
  , invalidSyntagma = not $ isValidSyntagma syn
  }
  where syn = fromMisoString inputField

updateModel GenerateSyntagmas g@Model {..} = g { generator = Generating } <# do
  syns <- fetchSyntagmas realNumber
  pure $ ShuffleSyns (syntagmas ++ map (\x -> (x, 0)) syns)
 where
  number = read (fromMisoString numberField) :: Int
  realNumber | (number + length syntagmas) > 50 = 50 - length syntagmas
             | number < 0                       = 0
             | otherwise                        = number

updateModel (UpdateTeamField   str) g = noEff g { inputField = str }
updateModel (UpdateSynField    str) g = noEff g { inputField = str }
updateModel (UpdateNumberField num) g = noEff g { numberField = num }
updateModel (UpdateTimerField  num) g = noEff g { timerField = num }


updateModel StartCounter g@Model {..} =
  effectSub g $ \sink -> forM_ [0 ..] $ \n -> do
    threadDelay 1000000
    sink Tick

updateModel Tick g@Model {..}
  | time <= timer && time > 1
  = noEff g { time          = time - 1
            , buttonEnabled = False
            , timeElapsed   = timeElapsed + 1
            }
  | time == 1
  = g { state         = TimeOut
      , time          = 0
      , currentTeam   = (currentTeam + 1) `mod` length teams
      , buttonEnabled = False
      , timeElapsed   = timeElapsed + 1
      }
    <# do
         shuff <- shuffleM syntagmas
         return $ ShuffleSyns shuff
  | otherwise
  = noEff g { buttonEnabled = False, timeElapsed = timeElapsed + 1 }

updateModel NoOp m = noEff m

fetchSyntagmas :: Int -> IO [String]
fetchSyntagmas n = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String [String] of
    Left  s -> error s -- TODO promijeniti
    Right j -> pure j
 where
  req = Request
    { reqMethod          = GET
    , reqURI             = ms $ "syns/" ++ show n
    , reqLogin           = Nothing
    , reqHeaders         = []
    , reqWithCredentials = False
    , reqData            = NoData
    }

postSyntagmas :: [(String, Int)] -> IO ()
postSyntagmas syntagmas = do
  _ <- contents <$> xhrByteString req
  return ()
 where
  req = Request
    { reqMethod          = POST
    , reqURI             = ms ("postSyns/" ++ tuplesToQuery syntagmas)
    , reqLogin           = Nothing
    , reqHeaders         = []
    , reqWithCredentials = False
    , reqData            = NoData
    }

tuplesToQuery :: [(String, Int)] -> String
tuplesToQuery []            = ""
tuplesToQuery [(s, n)     ] = s ++ " " ++ show n
tuplesToQuery ((s, n) : xs) = (s ++ " " ++ show n ++ " ") ++ tuplesToQuery xs

isValidSyntagma :: String -> Bool
isValidSyntagma syn = (length s == 2) && all (all isLetter) s
  where s = words syn

isValidTeam :: String -> [(String, Int)] -> Bool
isValidTeam team teams = not (null team) && (team, 0) `notElem` teams

updateTeams :: [(String, Int)] -> Int -> Int -> [(String, Int)]
updateTeams teamList idx n =
  take idx teamList ++ [updatedTeam] ++ drop (idx + 1) teamList
 where
  updatedTeam = (fst currentT, snd currentT + n)
  currentT    = teamList !! idx

sample1 :: [a] -> IO a
sample1 xs = do
  let l = length xs - 1
  idx <- randomRIO (0, l)
  return $ xs !! idx

sample :: Eq a => Int -> [a] -> IO [a]
sample 0 xs = return []
sample n xs = do
  let l = min n (length xs)
  val <- sample1 xs
  (:) <$> pure val <*> sample (l - 1) (delete val xs)
