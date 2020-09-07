{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.Show as S
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)

import Data.Text (pack, unpack, Text)
import Data.Map
import Data.Time.Clock (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime)

import Reflex
import Reflex.Dom

import Model
import SVG

main :: IO ()
main = mainWidgetWithHead docHead gubby

css :: Text
css = 
  ".slider-width { width: 270px; }"

docHead ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  m ()
docHead = do
  elAttr "meta" ("charset" =: "utf-8") $ blank
  elAttr "html" ("lang" =: "en-US") $ blank
  el "style" $ text css
  el "title" $ text "Gubby"

textMapInnerFloat :: String -> Float -> Map Text Text
textMapInnerFloat str fl =
  (pack str) =: (pack $ show fl) 

rangeInputConf :: Reflex t => RangeInputConfig t
rangeInputConf = def { 
    _rangeInputConfig_attributes =  
      constDyn $ textMapInnerFloat "step" 10 
      <> textMapInnerFloat "min" 30 
      <> textMapInnerFloat "max" 21600
      <> "class" =: "slider-width"
  , _rangeInputConfig_initialValue = 30.0
  }

gubby :: forall t m.
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  m ()
gubby = mdo
  el "h1" $ text "Gubby"
  text "A creature you take care of,"
  el "br" blank
  text "built with functional reactive programming,"
  el "br" blank 
  text "harboring a dark secret..."
  el "br" blank
  el "br" blank
  el "div" $ mdo
    dCreature <- fst <$> creatureNetwork gamePlayButtonsEvents gameStateButtonsEvents

    gameStateButtonsEvents <- gameStateButtons (paused <$> dCreature)

    gameView dCreature 

    gamePlayButtonsEvents <- gamePlayButtons dCreature
    
    el "br" blank

    text "Game Speed (Feed Every n Seconds):"

    el "br" blank

    dynText $ (pack . show) <$> _rangeInput_value rangeVal

    el "br" blank

    rangeVal <- rangeInput $ rangeInputConf

    el "br" blank
    el "br" blank

    dAge <- snd <$> creatureNetwork gamePlayButtonsEvents gameStateButtonsEvents

    dynText $ (pack . ("Age: " ++) . (++ " seconds") . show) <$> dAge

    el "br" blank
    el "br" blank
    el "br" blank
    el "br" blank

    debugView dCreature

    el "br" blank
    el "br" blank
    el "br" blank
    el "br" blank

    elAttr "a" (fromList [("href", "https://reflex-frp.org/"), ("target", "_blank") ]) (text "♥ Haskell & Reflex-FRP")

    el "br" blank
    el "br" blank

    elAttr "a" (fromList [("href", "https://github.com/wunderbrick/gubby"), ("target", "_blank") ]) (text "Source Code")

    return ()

debugView :: forall t m.
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  Dynamic t Creature ->
  m ()
debugView dCreature = do
  let 
    prettyView :: Show a => String -> (Creature -> a) -> m ()
    prettyView label accessor = do
      dynText $ ((pack . (label ++) . show) <$> accessor) <$> dCreature
      el "br" blank
  
  el "b" $ text "Debug: "
  el "br" blank
  prettyView "Stage: " stage
  prettyView "Consciousness: " consciousness
  prettyView "Poop State: " poopState
  prettyView "Appetite: " appetite
  prettyView "Food Journal: " foodJournal
  prettyView "Care Mistakes: " careMistakes
  prettyView "Creature Activity: " creatureActivity 
  prettyView "Paused: " paused

makeButton :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  Text ->
  m (Event t ())
makeButton txt = do
  (eC,_) <- elAttr' "button" mempty $ text txt
  return $ domEvent Click eC

timerWidget ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  Dynamic t Stage ->
  Dynamic t Bool ->
  (Event t (), Event t ()) ->
  m (Dynamic t Integer)
timerWidget dStage dPause gameStateButtonsEvents = do
  let (ePause, eRespawn) = gameStateButtonsEvents
  initTime <- liftIO getCurrentTime
  eTick <- tickLossy 1.0 initTime
  let eTickOnlyIfAliveAndNotEvil = gate ((\stage -> stage == Egg || stage == Stage1) <$> current dStage) eTick
  timer' dStage dPause ePause eTickOnlyIfAliveAndNotEvil

timer' ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Stage ->
  Dynamic t Bool ->
  Event t () -> 
  Event t TickInfo -> 
  m (Dynamic t Integer)
timer' dStage dPause ePause eTick = do
  let
    startOrPause :: Bool -> (Integer -> Integer)
    startOrPause p =
      if p then (0+) else (1+)

    bPause = startOrPause <$> current dPause

  bTimer <- hold never $ (tag bPause eTick) <$ ePause
        
  let eSwitch = switch bTimer
  foldDyn ($) 0 eSwitch

creatureNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  (Event t (), Event t (), Event t ()) ->
  (Event t (), Event t ()) ->
  m (Dynamic t Creature, Dynamic t Integer)
creatureNetwork gamePlayButtonsEvents gameStateButtonsEvents = mdo
  let (eFeedBanana, eFeedPlum, eScoop) = gamePlayButtonsEvents
      eFeed = leftmost [ LongFood <$ eFeedBanana, ShortFood <$ eFeedPlum ]
      (ePause, eRespawn) = gameStateButtonsEvents
  dTimer <- timerWidget dStage dPause gameStateButtonsEvents
  dStage <- stageNetwork dTimer dCareMistakes dFoodJournal
  dConsciousness <- consciousnessNetwork dTimer
  dPoopState <- poopStateNetwork dTimer eScoop
  dAppetite <- appetiteNetwork dTimer eFeed
  dFoodJournal <- foodJournalNetwork eFeed
  dCareMistakes <- careMistakesNetwork dTimer dConsciousness dAppetite dPoopState
  dCreatureActivity <- creatureActivityNetwork dTimer eFeed dPoopState eScoop
  dPause <- pauseNetwork ePause
  
  return 
    $ (Creature 
    <$> dStage
    <*> dConsciousness 
    <*> dPoopState 
    <*> dAppetite 
    <*> dFoodJournal 
    <*> dCareMistakes 
    <*> dCreatureActivity
    <*> dPause
    , dTimer)

pauseNetwork :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t () ->
  m (Dynamic t Bool)
pauseNetwork ePause = mdo
  foldDyn ($) True $ leftmost [ not <$ ePause ] 


stageNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  Dynamic t CareMistakes ->
  Dynamic t FoodJournal ->
  m (Dynamic t Stage)
stageNetwork dTimer dCareMistakes dFoodJournal = do
  let 
      hatch :: Stage -> Stage
      hatch Egg = Stage1
      hatch stage = stage

      kill :: Stage -> Stage
      kill _ = Dead

      makeEvil :: Stage -> Stage
      makeEvil _ = StageEvil
    
      bHatch = (\time -> time == 1) <$> current dTimer
      eHatch = hatch <$ gate bHatch (updated dTimer) -- hatch 1 second after pressing start initially
      
      bKill = (\careMistakes -> length careMistakes >= 6) <$> current dCareMistakes
      eKill = kill <$ gate bKill (updated dTimer) -- kill if too many care mistakes
      
      bEvil = (\foodJournal -> foodJournal == goEvil) <$> current dFoodJournal
      eGoEvil = makeEvil <$ gate bEvil (updated dTimer) -- evil if we have FML in Morse Code via LongFood/ShortFood
  
  foldDyn ($) Egg $ leftmost [ eHatch, eKill, eGoEvil ]

consciousnessNetwork :: forall t m.
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  m (Dynamic t Consciousness)
consciousnessNetwork dTimer = mdo
  let 
    putToSleep :: Consciousness -> Consciousness
    putToSleep con = if con == Awake then Asleep else con

    wakeUp :: Consciousness -> Consciousness
    wakeUp con = if con == Asleep then Awake else con

    isAsleep :: Consciousness -> Bool
    isAsleep con =
      con == Asleep

    bPutToSleep = isItTime 170 <$> (current dTimer)
    ePutToSleep = putToSleep <$ gate bPutToSleep (updated dTimer) 

    bWakeUp = isAsleep <$> current consc
    eWakeUp = wakeUp <$ gate bWakeUp (updated dTimer) 

  consc <- foldDyn ($) Awake $ leftmost [ eWakeUp, ePutToSleep ]

  return consc

isItTime :: Integer -> Integer -> Bool
isItTime howOften currentTime =
  currentTime /= 0 && (currentTime == howOften || mod currentTime howOften == 0)

poopStateNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  Event t () ->
  m (Dynamic t PoopState)
poopStateNetwork dTimer eScoop = do
  let
    ePoop = poopEvent dTimer

    scoopPoop :: PoopState -> PoopState
    scoopPoop ps =
      if ps == PoopPresent then NoPoop else ps

    eScoopPoop = scoopPoop <$ eScoop

  foldDyn ($) NoPoop $ leftmost [ eScoopPoop, ePoop ] 

poopEvent ::
  Reflex t =>
  Dynamic t Integer ->
  Event t (PoopState -> PoopState)
poopEvent dTimer = 
  let
    makePoop :: PoopState -> PoopState
    makePoop ps =
      if ps == NoPoop then PoopPresent else ps

    bMakePoop = isItTime 70 <$> (current dTimer)
  in 
    makePoop <$ gate bMakePoop (updated dTimer)

appetiteNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  Event t (Food) ->
  m (Dynamic t Appetite)
appetiteNetwork dTimer eFeed = do
  let
    eMakeHungry = hungerEvent dTimer

    eFeedCreature = fillStomach <$> eFeed 

  foldDyn ($) Full $ leftmost [ eFeedCreature, eMakeHungry ]

hungerEvent ::
  Reflex t =>
  Dynamic t Integer ->
  Event t (Appetite -> Appetite)
hungerEvent dTimer =
  let
    makeHungry :: Appetite -> Appetite
    makeHungry app =
      if app == Full then Hungry (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) else app

    bMakeHungry = isItTime 30 <$> (current dTimer)
  in 
    makeHungry <$ gate bMakeHungry (updated dTimer)

foodJournalNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t (Food) ->
  m (Dynamic t FoodJournal)
foodJournalNetwork eFeed = do
  foldDyn 
    ($) (FoodJournal Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) 
    $ journalFood <$> eFeed

careMistakesNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  Dynamic t Consciousness ->
  Dynamic t Appetite ->  
  Dynamic t PoopState ->
  m (Dynamic t CareMistakes)
careMistakesNetwork dTimer dConsciousness dAppetite dPoopState = mdo
  let
    addCareMistake :: CareMistake -> CareMistakes -> CareMistakes
    addCareMistake cm cms = 
      cms ++ [ cm ] 

    bAwake = (\consc -> consc == Awake) <$> current dConsciousness

    bPoopState = (\ps -> ps == PoopPresent) <$> current dPoopState

    bAwakeAndPoopPresent = (&&) <$> bAwake <*> bPoopState

    ePoopCareMistake = addCareMistake TooMuchPoop <$ (gate bAwakeAndPoopPresent $ poopEvent dTimer) 

    bHungerState = (\hs -> hs /= Full) <$> current dAppetite

    bAwakeAndHungry = (&&) <$> bAwake <*> bHungerState

    eHungerCareMistake = addCareMistake Hunger <$ (gate bAwakeAndHungry $ hungerEvent dTimer)

  foldDyn ($) [] $ leftmost [ ePoopCareMistake, eHungerCareMistake ]

creatureActivityNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  Event t Food ->
  Dynamic t PoopState ->
  Event t () ->
  m (Dynamic t CreatureActivity)
creatureActivityNetwork dTimer eFeed dPoopState eScoop = do
  eThreeSecondsAfterFeeding <- delay 3 eFeed

  let 
    eat :: Food -> CreatureActivity -> CreatureActivity
    eat food (Eating _) = (Eating food) -- silly but it works
    eat food Frolicking = (Eating food)

    frolick :: CreatureActivity -> CreatureActivity
    frolick _ = Frolicking

    avoidPoop :: CreatureActivity -> CreatureActivity
    avoidPoop _ = AvoidingPoop

    poopPresent :: PoopState -> Bool
    poopPresent ps =
      ps == PoopPresent

    bPoopPresent = poopPresent <$> current dPoopState

    eGoFromEatingBackToFrolicking = frolick <$ eThreeSecondsAfterFeeding
    goFromAvoidingPoopBackToFrolicking = frolick <$ eScoop
    eEat = eat <$> eFeed
    eAvoidPoop = avoidPoop <$ updated dPoopState

  foldDyn ($) Frolicking $ leftmost [ eEat, eGoFromEatingBackToFrolicking, goFromAvoidingPoopBackToFrolicking, eAvoidPoop ]

determineView :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>  
  Dynamic t Creature -> 
  m (Event t ())
determineView dCreature = 
  dyn 
  $ determineView' 
  <$> (stage <$> dCreature) 
  <*> (consciousness <$> dCreature) 
  <*> (poopState <$> dCreature)
  <*> (creatureActivity <$> dCreature) 
  <*> (appetite <$> dCreature)
  <*> (paused <$> dCreature)

determineView' :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  Stage -> 
  Consciousness -> 
  PoopState -> 
  CreatureActivity ->
  Appetite ->
  Bool ->
  m ()
determineView' stage consciousness poopState act appetite paused =
  case stage of
    Egg -> eggSVG
    Stage1 ->
      case paused of
        False -> 
          case consciousness of
            Asleep ->
              case poopState of
                PoopPresent -> sleepingNextToPoopSVG
                NoPoop -> sleepingSVG 
            Awake ->
              case act of
                Frolicking -> 
                  case appetite of 
                    Full -> frolickingSVG
                    Hungry _ -> hungryFrolickingSVG
                Eating food -> eatingSVG food
                AvoidingPoop -> avoidingPoopSVG
        True -> el "h2" $ text "PAUSED"
    Dead -> deadSVG
    StageEvil -> stageEvilSVG

gameView ::
  MonadWidget t m =>
  Dynamic t Creature ->
  m ()
gameView dCreature =
  el "div" $ mdo
    determineView dCreature
    el "br" blank
    el "br" blank

hideFeedingElements :: Appetite -> PoopState -> Consciousness -> Stage -> CreatureActivity -> Bool
hideFeedingElements appetite poopState consciousness stage activity =
  appetite == Full || poopState == PoopPresent || consciousness == Asleep || stage /= Stage1 || activity == Eating LongFood || activity == Eating ShortFood

hideScoopElement :: PoopState -> Consciousness -> Stage -> Bool
hideScoopElement poopState consciousness stage =
  poopState == NoPoop || consciousness == Asleep || stage /= Stage1

disable :: Bool -> Map Text Text
disable True  = singleton "disabled" ""
disable False = mempty

gameStateButtons ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Bool ->
  m (Event t (), Event t ())
gameStateButtons dPause = mdo
  let
    pauseButtonText :: Bool -> Text
    pauseButtonText b =
      if b then "Start" else "Pause"

  (eP,_) <- elDynAttr' "button" mempty $ dynText $ pauseButtonText <$> dPause
  let ePause = domEvent Click eP

  (eRs,_) <- elDynAttr' "button" mempty $ text "Respawn"
  let eRespawn = domEvent Click eRs

  el "br" blank
  el "br" blank

  return (ePause, eRespawn)

gamePlayButtons ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Creature -> 
  m (Event t (), Event t (), Event t ())
gamePlayButtons dCreature = mdo
  let dHunger = appetite <$> dCreature
      dStage = stage <$> dCreature
      dConsciousness = consciousness <$> dCreature
      dActivity = creatureActivity <$> dCreature
      dHungerBool = hideFeedingElements <$> dHunger <*> dPoopState <*> dConsciousness <*> dStage <*> dActivity
      dFeedDisabledOrNot = disable <$> dHungerBool
      dPoopState = poopState <$> dCreature
      dPoopStateBool = hideScoopElement <$> dPoopState <*> dConsciousness <*> dStage
      dScoopDisabledOrNot = disable <$> dPoopStateBool

  (eFB,_) <- elDynAttr' "button" dFeedDisabledOrNot $ text "Feed Banana"
  let eFeedBanana = domEvent Click eFB

  (eFP,_) <- elDynAttr' "button" dFeedDisabledOrNot $ text "Feed Plum"
  let eFeedPlum = domEvent Click eFP

  (eSP,_) <- elDynAttr' "button" dScoopDisabledOrNot $ text "Scoop Poop"
  let eScoop = domEvent Click eSP

  el "br" blank
  el "br" blank

  return (eFeedBanana, eFeedPlum, eScoop)

-- add meals until full
fillStomach :: Food -> Appetite -> Appetite
fillStomach food Full = Full
fillStomach food (Hungry (slot0, slot1, slot2, slot3, slot4, slot5)) = 
    fillStomach' food (Hungry (slot0, slot1, slot2, slot3, slot4, slot5))

fillStomach' :: Food -> Appetite -> Appetite
fillStomach' food (Hungry (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)) = 
    Hungry (Just food, Nothing, Nothing, Nothing, Nothing, Nothing)

fillStomach' food (Hungry (full0, Nothing, Nothing, Nothing, Nothing, Nothing)) = 
    Hungry (full0, Just food, Nothing, Nothing, Nothing, Nothing)

fillStomach' food (Hungry (full0, full1, Nothing, Nothing, Nothing, Nothing)) = 
    Hungry (full0, full1, Just food, Nothing, Nothing, Nothing)

fillStomach' food (Hungry (full0, full1, full2, Nothing, Nothing, Nothing)) = 
    Hungry (full0, full1, full2, Just food, Nothing, Nothing)

fillStomach' food (Hungry (full0, full1, full2, full3, Nothing, Nothing)) = 
    Hungry (full0, full1, full2, full3, Just food, Nothing)

fillStomach' food (Hungry (full0, full1, full2, full3, full4, Nothing)) = 
    Full
fillStomach' food Full = 
    Full

-- keep track of last 10 meals to potentially initiate evil mode
journalFood :: Food -> FoodJournal -> FoodJournal
journalFood food (FoodJournal Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) = 
    FoodJournal (Just food) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

journalFood food (FoodJournal one Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) = 
    FoodJournal (Just food) one Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

journalFood food (FoodJournal one two Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) = 
    FoodJournal (Just food) one two Nothing Nothing Nothing Nothing Nothing Nothing Nothing

journalFood food (FoodJournal one two three Nothing Nothing Nothing Nothing Nothing Nothing Nothing) = 
    FoodJournal (Just food) one two three Nothing Nothing Nothing Nothing Nothing Nothing

journalFood food (FoodJournal one two three four Nothing Nothing Nothing Nothing Nothing Nothing) = 
    FoodJournal (Just food) one two three four Nothing Nothing Nothing Nothing Nothing

journalFood food (FoodJournal one two three four five Nothing Nothing Nothing Nothing Nothing) = 
    FoodJournal (Just food) one two three four five Nothing Nothing Nothing Nothing

journalFood food (FoodJournal one two three four five six Nothing Nothing Nothing Nothing) = 
    FoodJournal (Just food) one two three four five six Nothing Nothing Nothing

journalFood food (FoodJournal one two three four five six seven Nothing Nothing Nothing) = 
    FoodJournal (Just food) one two three four five six seven Nothing Nothing

journalFood food (FoodJournal one two three four five six seven eight Nothing Nothing) = 
    FoodJournal (Just food) one two three four five six seven eight Nothing

journalFood food (FoodJournal one two three four five six seven eight nine Nothing) = 
    FoodJournal (Just food) one two three four five six seven eight nine

journalFood food (FoodJournal one two three four five six seven eight nine ten) = 
    FoodJournal (Just food) one two three four five six seven eight nine