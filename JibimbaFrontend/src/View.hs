-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module View
  ( viewModel
  )
where

-- | Miso framework import
import           Miso                    hiding ( on )
import           Miso.String                    ( MisoString
                                                , ms
                                                )

import           Model

import           Data.Char                      ( toUpper )
import qualified Data.Map                      as M
import           Data.Bool                      ( bool )
import           Data.Function                  ( on )
import           Data.List                      ( sortBy )

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel g@Model {..} = div_
  [class_ "mainDiv sredina"]
  [ section_
      []
      [ img_ [src_ "./logo-320.png"]
      , case state of -- MANUAL ROUTING
        MainMenu  -> viewMainMenu g

        Settings  -> viewSettings g timerField

        TeamInput -> viewTeamInput g inputField

        SynInput  -> viewSynInput g inputField numberField

        RoundPrep -> viewRoundPrep g

        Gameplay  -> viewGameplay g

        TimeOut   -> viewTimeOut g

        GameOver  -> viewGameOver g

        _         -> viewRules g
      ]
  -- , div_ [] [
  --     text "----------------------------------", br_ []
  --   , ul_ [] $ flip map (syntagmas) $ \t ->
  --       li_ [] [ text (ms (fst t ++ " " ++ show (snd t))) ]
  --   , ul_ [] $ flip map (guessed) $ \t ->
  --       li_ [] [ text (ms (fst t ++ " " ++ show (snd t))) ]
  --   ]
  ]


viewMainMenu :: Model -> View Action
viewMainMenu g@Model {..} = ul_
  [class_ "mainMenuList"]
  [ li_
    []
    [button_ [class_ "regularButton", onClick ToTeamInput] [text "NEW GAME"]]
  , li_
    []
    [button_ [class_ "regularButton", onClick ToBasicRules] [text "RULES"]]
  , li_
    []
    [button_ [class_ "regularButton", onClick ToSettings] [text "SETTINGS"]]
  ]


viewRules :: Model -> View Action
viewRules g@Model {..} = div_
  []
  [ div_
    []
    [ button_ [class_ "rulesButton", onClick ToBasicRules] [text "BASIC "]
    , button_ [class_ "rulesButton", onClick ToAliasRules] [text "ALIAS   "]
    ]
  , div_
    []
    [ button_ [class_ "rulesButton", onClick ToCharadesRules] [text "CHARADES"]
    , button_ [class_ "rulesButton", onClick ToOneWordRules]  [text "ONE WORD"]
    ]
  , case state of
    BasicRules    -> viewBasicRules
    AliasRules    -> viewAliasRules
    CharadesRules -> viewCharadesRules
    OneWordRules  -> viewOneWordRules
  , div_
    [class_ "bottomButtons"]
    [button_ [class_ "regularButton", onClick BackToMainMenu] [text "BACK"]]
  ]

viewBasicRules :: View Action
viewBasicRules = div_
  []
  [ p_
    []
    [ text "There are three rounds:"
    , br_ []
    , text "1. Alias"
    , br_ []
    , text "2. Charades"
    , br_ []
    , text "3. One word"
    , br_ []
    ]
  , p_
    []
    [ text "The word pairs are repeating in every round."
    , br_ []
    , text "PAIR = ADJECTIVE + NOUN"
    ]
  ]

viewAliasRules :: View Action
viewAliasRules = div_
  []
  [ p_ [] [text "Use words to describe the pair."]
  , p_ [] [text "No foreign languages or using words with the same root."]
  , p_ [] [text "For more challenge, no body language."]
  ]

viewCharadesRules :: View Action
viewCharadesRules = div_
  []
  [ p_ [] [text "Use body language and movement to describe the pair."]
  , p_ [] [text "Be silent."]
  , p_ []
       [text "For more challenge, don't point to things that are around you."]
  ]

viewOneWordRules :: View Action
viewOneWordRules = div_
  []
  [ p_ [] [text "Use only ONE word to describe the pair."]
  , p_ [] [text "No foreign languages or using words with the same root."]
  , p_ [] [text "For more challenge, no body language."]
  ]

viewSettings :: Model -> MisoString -> View Action
viewSettings g@Model {..} num = div_
  []
  [ div_
    [class_ "settings"]
    [ text "Time: "
    , input_
      [ type_ "number"
      , autofocus_ True
      , defaultValue_ $ ms timer
      , value_ num
      , min_ "10"
      , max_ "120"
      , step_ "10"
      , name_ "timeSettings"
      , onInput UpdateTimerField
      ]
    , text " seconds (10-120)"
    ]
  , div_
    [class_ "bottomButtons"]
    [ button_ [class_ "regularButton", onClick SettingsToMainMenu]
              [text "MAIN MENU"]
    ]
  ]

viewTeamInput :: Model -> MisoString -> View Action
viewTeamInput g@Model {..} teamName = div_
  []
  [ div_ []
         [text "Please insert team names.", br_ [], text "Minimally 2 teams!"]
  , div_
    [ style_
      (M.singleton "visibility" (bool "hidden" "visible" invalidTeamName))
    , class_ "backButton"
    ]
    [text "You've entered an empty team name or a name that already exists!"]
  , div_
    []
    [ input_
        [ placeholder_ "TeamName"
        , type_ "text"
        , autofocus_ True
        , maxlength_ "15"
        , value_ teamName
        , name_ "newTeamName"
        , onInput UpdateTeamField
        , onEnter AddTeam
        ]
    ]
  , ul_ [] $ flip map (take 4 teams) $ \t -> li_ [] [text (ms (fst t))]
  , div_
    [class_ "bottomButtons"]
    [ button_ [class_ "backButton", onClick BackToMainMenu] [text "BACK"]
    , button_ [class_ "forwardButton", onClick ToSynInput]  [text "NEXT"]
    ]
  ]

viewSynInput :: Model -> MisoString -> MisoString -> View Action
viewSynInput g@Model {..} syn num = div_
  []
  [ div_
    []
    [ span_ [] [text "Number of pairs: "]
    , span_
      [ style_
          (M.singleton "color"
                       (bool "#EC676E" "#18BC9C" (0 /= length syntagmas))
          )
      ]
      [text (ms (show (length syntagmas)))]
    , span_ [] [text " (max 50)"]
    ]
  , div_
    [ style_
      (M.singleton "visibility" (bool "hidden" "visible" invalidSyntagma))
    , class_ "backButton"
    ]
    [text "A pair can only contain letters"]
  , div_ [] [text "Enter pair..."]
  , input_
    [ placeholder_ "Semantical deviance"
    , type_ "text"
    , autofocus_ True
    , value_ syn
    , name_ "newSyn"
    , onInput UpdateSynField
    , onEnter AddSyn
    ]
  , br_ []
  , br_ []
  , div_
    []
    [ text "... or a number of pairs to be generated."
    , br_ []
    , input_
      [ type_ "number"
      , defaultValue_ "30"
      , autofocus_ False
      , value_ num
      , min_ "1"
      , max_ "50"
      , name_ "numberToGenerate"
      , onInput UpdateNumberField
      , onEnter GenerateSyntagmas
      ]
    , button_ [class_ "regularButton", onClick GenerateSyntagmas]
              [text "GENERATE"]
    ]
  , span_
    [ style_
        (M.fromList
          [ ("visibility", bool "visible" "hidden" (generator == Idle))
          , ("color"     , bool "#2C3E50" "#18BC9C" (generator == Done))
          ]
        )
    ]

    [bool "Generating..." "Done!" (generator == Done)]
  , div_
    [class_ "bottomButtons"]
    [ button_ [class_ "backButton", onClick ToTeamInput]    [text "BACK"]
    , button_ [class_ "forwardButton", onClick ToRoundPrep] [text "START GAME"]
    ]
  ]

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool NoOp action . (== KeyCode 13)

viewRoundPrep :: Model -> View Action
viewRoundPrep g@Model {..} = div_
  []
  [ div_
    []
    [ text (ms ("Round: " ++ show runda))
    , br_ []
    , div_ [style_ (M.singleton "color" "#18BC9C")]
           [displayRoundDescription runda]
    , viewTeamScores teams
    , text
      (ms
        (  "Team "
        ++ map toUpper (fst (teams !! currentTeam))
        ++ ", are you ready? You have "
        ++ show timer
        ++ " seconds!"
        )
      )
    , br_ []
    ]
  , div_
    [class_ "bottomButtons"]
    [ button_ [class_ "backButton", onClick ToSynInput] [text "BACK"]
    , button_ [autofocus_ True, class_ "forwardButton", onClick ToGameplay]
              [text "READY!"]
    ]
  ]

displayRoundDescription :: Int -> View Action
displayRoundDescription 1 = text "ALIAS"
displayRoundDescription 2 = text "CHARADES"
displayRoundDescription 3 = text "ONE WORD"
displayRoundDescription _ = text "TO BE IMPLEMENTED"

viewGameplay :: Model -> View Action
viewGameplay g@Model {..} = div_
  []
  [ div_
    [class_ "syntagma sredina"]
    [ text (ms (head (words (fst (head syntagmas)))))
    , br_ []
    , text (ms (last (words (fst (head syntagmas)))))
    , br_ []
    , text (ms (show time))
    ]
  , div_
    [class_ "bottomButtons"]
    [ button_ [disabled_ buttonEnabled, class_ "backButton", onClick SkipWord]
              [text "SKIP!"]
    , button_
      [ disabled_ buttonEnabled
      , autofocus_ True
      , class_ "forwardButton"
      , onClick NextWord
      ]
      [text "BINGO!"]
    ]
  ]

viewTimeOut :: Model -> View Action
viewTimeOut g@Model {..} = div_
  []
  [ div_
    []
    [ text "Time's up!"
    , br_ []
    , viewTeamScores teams
    , text
      (ms
        (  "Team "
        ++ map toUpper (fst (teams !! currentTeam))
        ++ ", are you ready? You have "
        ++ show timer
        ++ " seconds!"
        )
      )
    , br_ []
    ]
  , div_
    [class_ "bottomButtons"]
    [ button_ [autofocus_ True, class_ "regularButton", onClick ToGameplay]
              [text "READY!"]
    ]
  ]

viewGameOver :: Model -> View Action
viewGameOver g@Model {..} = div_
  []
  [ div_ [] [text "Game over!", br_ [], viewTeamScores teams]
  , div_
    [class_ "bottomButtons"]
    [ button_ [autofocus_ True, class_ "regularButton", onClick PostSyntagmas]
              [text "MAIN MENU"]
    ]
  ]

top5 :: [(String, Int)] -> [(String, Int)]
top5 = take 4 . sortBy (flip compare `on` snd)

viewTeamScores :: [(String, Int)] -> View Action
viewTeamScores teams = div_
  []
  [ ul_ [] $ flip map (top5 teams) $ \t ->
    li_ [] [text (ms (fst t ++ ": " ++ show (snd t)))]
  , br_ []
  ]
