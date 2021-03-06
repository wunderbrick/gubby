{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.Show as S
import Control.Monad (void, join, (<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Text (pack, unpack, Text)
import Data.Map
import Data.Time.Clock (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime)

import Reflex
import Reflex.Network
import Reflex.Dom

import Model
import SVG

main :: IO ()
main = mainWidgetWithHead docHead gubby

css :: Text
css = 
  ".slider-width { width: 270px; }"

br ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  m ()
br = do 
  el "br" blank

nbr ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Int -> m [()]
-- multiple line breaks
nbr n =
  sequence brs 
    where brs = replicate n br

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
      constDyn $ textMapInnerFloat "step" 1 
      <> textMapInnerFloat "min" 1 
      <> textMapInnerFloat "max" 720
      <> "class" =: "slider-width"
  , _rangeInputConfig_initialValue = 1
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
  br
  text "built with functional reactive programming,"
  br 
  text "harboring a dark secret..."
  nbr 2
  el "div" $ mdo
    dCreature <- fst <$> creatureNetwork gamePlayButtonsEvents gameStateButtonsEvents dRangeNum

    gameStateButtonsEvents <- gameStateButtons (paused <$> dCreature) (stage <$> dCreature)

    gameView dCreature 

    gamePlayButtonsEvents <- gamePlayButtons dCreature
    
    br

    text "Game Speed (Feed Every n Seconds):"

    br

    dynText $ (pack . show . (*15)) <$> dRangeNum

    br

    rangeVal <- rangeInput $ rangeInputConf

    let dRangeNum = round <$> _rangeInput_value rangeVal

    nbr 2

    dAge <- snd <$> creatureNetwork gamePlayButtonsEvents gameStateButtonsEvents dRangeNum

    dynText $ (pack . ("Age: " ++) . (++ " Seconds") . show) <$> dAge

    nbr 4

    {-
    debugView dCreature

    br
    br
    br
    br
    -}

    elAttr "a" (fromList [("href", "https://reflex-frp.org/"), ("target", "_blank") ]) (text "♥ Haskell & Reflex-FRP")

    nbr 2

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
      br
  
  el "b" $ text "Debug: "
  br
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
  timer' dStage dPause ePause eTickOnlyIfAliveAndNotEvil eRespawn

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
  Event t () ->
  m (Dynamic t Integer)
timer' dStage dPause ePause eTick eRespawn = do
  let
    startOrPause :: Bool -> (Integer -> Integer)
    startOrPause p =
      if p then (0+) else (1+)

    bPause = startOrPause <$> current dPause

  bTimer <- hold never $ (tag bPause eTick) <$ ePause
        
  let eSwitch = switch bTimer
  foldDyn ($) 0 $ leftmost [ eSwitch, (0*) <$ eRespawn ]

creatureNetwork :: forall t m.
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  (Event t (), Event t (), Event t ()) ->
  (Event t (), Event t ()) ->
  Dynamic t Integer ->
  m (Dynamic t Creature, Dynamic t Integer)
creatureNetwork gamePlayButtonsEvents gameStateButtonsEvents dRangeVal = mdo
  let (eFeedBanana, eFeedPlum, eScoop) = gamePlayButtonsEvents
      eFeed = leftmost [ LongFood <$ eFeedBanana, ShortFood <$ eFeedPlum ]
      (ePause, eRespawn) = gameStateButtonsEvents 
  dTimer <- timerWidget dStage dPause gameStateButtonsEvents
  dStage <- stageNetwork dTimer dCareMistakes dFoodJournal eRespawn
  dConsciousness <- consciousnessNetwork dTimer eRespawn dRangeVal
  dPoopState <- poopStateNetwork dTimer eScoop eRespawn dRangeVal
  dAppetite <- appetiteNetwork dTimer eFeed eRespawn dRangeVal
  dFoodJournal <- foodJournalNetwork eFeed eRespawn
  dCareMistakes <- careMistakesNetwork dTimer dConsciousness dAppetite dPoopState eRespawn dRangeVal
  dCreatureActivity <- creatureActivityNetwork dTimer eFeed dPoopState eScoop eRespawn
  dPause <- pauseNetwork ePause eRespawn

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
  Event t () ->  
  m (Dynamic t Bool)
pauseNetwork ePause eRespawn = mdo
  foldDyn ($) True $ leftmost [ not <$ ePause, (\p -> True) <$ eRespawn ] 

stageNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  Dynamic t CareMistakes ->
  Dynamic t FoodJournal ->
  Event t () ->
  m (Dynamic t Stage)
stageNetwork dTimer dCareMistakes dFoodJournal eRespawn = do
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
  
  foldDyn ($) Egg $ leftmost [ (\s -> Egg ) <$ eRespawn, eHatch, eKill, eGoEvil ]

consciousnessNetwork :: forall t m.
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  Event t () ->
  Dynamic t Integer ->
  m (Dynamic t Consciousness)
consciousnessNetwork dTimer eRespawn dRangeVal = mdo
  let 
    putToSleep :: Consciousness -> Consciousness
    putToSleep con = if con == Awake then Asleep else con

    wakeUp :: Consciousness -> Consciousness
    wakeUp con = if con == Asleep then Awake else con

    isAsleep :: Consciousness -> Bool
    isAsleep con =
      con == Asleep

    bWakeUp = isAsleep <$> current consc

    ePutToSleep = timedEvent 70 putToSleep dTimer dRangeVal
    
    eWakeUp = timedEvent 30 wakeUp dTimer dRangeVal

    eWakeUpOnlyIfAsleep = gate bWakeUp eWakeUp
    
  consc <- foldDyn ($) Awake $ leftmost [ eWakeUp, ePutToSleep, (\c -> Awake )<$ eRespawn ]

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
  Event t () ->
  Dynamic t Integer ->
  m (Dynamic t PoopState)
poopStateNetwork dTimer eScoop eRespawn dRangeVal = do
  let
    scoopPoop :: PoopState -> PoopState
    scoopPoop ps =
      if ps == PoopPresent then NoPoop else ps

    eScoopPoop = scoopPoop <$ eScoop

  ePoop <- poopEvent dTimer dRangeVal

  foldDyn ($) NoPoop $ leftmost [ eScoopPoop, ePoop, (\ps -> NoPoop )<$ eRespawn ] 

poopEvent :: forall t m.
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  Dynamic t Integer ->
  m (Event t (PoopState -> PoopState))
poopEvent dTimer dRangeVal = mdo
  let
    makePoop :: PoopState -> PoopState
    makePoop ps =
      if ps == NoPoop then PoopPresent else ps
  
  return $ timedEvent 25 makePoop dTimer dRangeVal

appetiteNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Integer ->
  Event t (Food) ->
  Event t () ->
  Dynamic t Integer ->
  m (Dynamic t Appetite)
appetiteNetwork dTimer eFeed eRespawn dRangeVal = do
  let
    eMakeHungry = hungerEvent dTimer dRangeVal

    eFeedCreature = fillStomach <$> eFeed 

  foldDyn ($) Full $ leftmost [ eFeedCreature, eMakeHungry, (\a -> Full )<$ eRespawn ]

timedEvent :: forall t a b.
  Reflex t =>
  Integer ->
  (a -> a) ->
  Dynamic t Integer ->
  Dynamic t Integer ->
  Event t (a -> a)
timedEvent defaultInterval f dTimer dRangeVal =
  let
    bRangeVal = current dRangeVal

    bDefaultInterval = (constant defaultInterval :: Behavior t Integer)

    bAdjustedInterval = (*) <$> bRangeVal <*> bDefaultInterval

    bDoTheThing = isItTime <$> bAdjustedInterval <*> (current dTimer)
  in 
    f <$ gate bDoTheThing (updated dTimer) 

hungerEvent :: forall t.
  Reflex t =>
  Dynamic t Integer ->
  Dynamic t Integer ->
  Event t (Appetite -> Appetite)
hungerEvent dTimer dRangeVal =
  let
    makeHungry :: Appetite -> Appetite
    makeHungry app =
      if app == Full then Hungry (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) else app
  in
    timedEvent 15 makeHungry dTimer dRangeVal

foodJournalNetwork ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t (Food) ->
  Event t () -> 
  m (Dynamic t FoodJournal)
foodJournalNetwork eFeed eRespawn = do
  let initFJ = (FoodJournal Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  foldDyn ($) initFJ $ leftmost [ journalFood <$> eFeed, (\fj -> initFJ ) <$ eRespawn ]

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
  Event t () ->
  Dynamic t Integer ->
  m (Dynamic t CareMistakes)
careMistakesNetwork dTimer dConsciousness dAppetite dPoopState eRespawn dRangeVal = mdo
  ePoop <- poopEvent dTimer dRangeVal
  
  let
    addCareMistake :: CareMistake -> CareMistakes -> CareMistakes
    addCareMistake cm cms = 
      cms ++ [ cm ] 

    bAwake = (\consc -> consc == Awake) <$> current dConsciousness

    bPoopState = (\ps -> ps == PoopPresent) <$> current dPoopState

    bAwakeAndPoopPresent = (&&) <$> bAwake <*> bPoopState

    ePoopCareMistake = addCareMistake TooMuchPoop <$ (gate bAwakeAndPoopPresent $ ePoop) 

    bHungerState = (\hs -> hs /= Full) <$> current dAppetite

    bAwakeAndHungry = (&&) <$> bAwake <*> bHungerState

    eHungerCareMistake = addCareMistake Hunger <$ (gate bAwakeAndHungry $ hungerEvent dTimer dRangeVal)

  foldDyn ($) [] $ leftmost [ ePoopCareMistake, eHungerCareMistake, (\cms -> []) <$ eRespawn ]

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
  Event t () ->
  m (Dynamic t CreatureActivity)
creatureActivityNetwork dTimer eFeed dPoopState eScoop eRespawn = do
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
    eGoFromAvoidingPoopBackToFrolicking = frolick <$ eScoop
    eEat = eat <$> eFeed
    eAvoidPoop = avoidPoop <$ updated dPoopState -- in the leftmost below respawn has to be to the left of eAvoidPoop or it the updated dynamic poop state will trigger avoiding poop

  foldDyn ($) Frolicking $ leftmost [ eEat, eGoFromEatingBackToFrolicking, eGoFromAvoidingPoopBackToFrolicking, (\ca -> Frolicking) <$ eRespawn, eAvoidPoop ]

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
gameView dCreature = mdo
  el "div" $ determineView dCreature
  nbr 2
  return ()

hidePause :: Stage -> Bool
hidePause stage =
  stage == StageEvil

hideRespawn :: Stage -> Bool
hideRespawn stage =
  stage == Egg

disableFeedingElements :: Appetite -> PoopState -> Consciousness -> Stage -> CreatureActivity -> Bool
disableFeedingElements appetite poopState consciousness stage activity =
  appetite == Full || poopState == PoopPresent || consciousness == Asleep || stage /= Stage1 || activity == Eating LongFood || activity == Eating ShortFood

disableScoopElement :: PoopState -> Consciousness -> Stage -> Bool
disableScoopElement poopState consciousness stage =
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
  Dynamic t Stage ->
  m (Event t (), Event t ())
gameStateButtons dPause dStage = mdo
  let
    dPauseBool = hidePause <$> dStage
    dPauseButtonActiveOrNot = disable <$> dPauseBool

    dRespawnBool = hideRespawn <$> dStage
    dRespawnActiveOrNot = disable <$> dRespawnBool

    pauseButtonText :: Bool -> Text
    pauseButtonText b =
      if b then "Start" else "Pause"

  (eP,_) <- elDynAttr' "button" dPauseButtonActiveOrNot $ dynText $ pauseButtonText <$> dPause
  let ePause = domEvent Click eP

  (eRs,_) <- elDynAttr' "button" dRespawnActiveOrNot $ text "Respawn"
  let eRespawn = domEvent Click eRs

  br
  br

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
      dHungerBool = disableFeedingElements <$> dHunger <*> dPoopState <*> dConsciousness <*> dStage <*> dActivity
      dFeedDisabledOrNot = disable <$> dHungerBool
      dPoopState = poopState <$> dCreature
      dPoopStateBool = disableScoopElement <$> dPoopState <*> dConsciousness <*> dStage
      dScoopDisabledOrNot = disable <$> dPoopStateBool

  (eFB,_) <- elDynAttr' "button" dFeedDisabledOrNot $ text "Feed Banana"
  let eFeedBanana = domEvent Click eFB

  (eFP,_) <- elDynAttr' "button" dFeedDisabledOrNot $ text "Feed Plum"
  let eFeedPlum = domEvent Click eFP

  (eSP,_) <- elDynAttr' "button" dScoopDisabledOrNot $ text "Scoop Poop"
  let eScoop = domEvent Click eSP

  br
  br

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
