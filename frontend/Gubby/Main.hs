{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
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
import StateMachine
import SVG

main :: IO ()
main = mainWidgetWithHead docHead gubby

docHead ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  m ()
docHead = do
  elAttr "meta" (singleton "charset" "utf-8") $ blank
  elAttr "html" (singleton "lang" "en-US") $ blank
  el "title" $ text "Gubby"

gubby ::
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
    events <- makeEvents dCreature topLevelButtonEvents

    -- our game state (update is the "controller" function that uses a fold)
    dCreature <- updateCreature initCreature events

    -- our view function that takes the updated game    
    gameView dCreature 

    -- let's play with decoupling view (topLevelButtons returns events as well as creates DOM elements)
    topLevelButtonEvents <- topLevelButtons dCreature
    
    el "br" blank
    el "br" blank
    el "br" blank
    el "br" blank

    elAttr "a" (fromList [("href", "https://reflex-frp.org/"), ("target", "_blank") ]) (text "♥ Haskell & Reflex-FRP")

    el "br" blank
    el "br" blank

    elAttr "a" (fromList [("href", "https://github.com/wunderbrick/gubby"), ("target", "_blank") ]) (text "Source Code")

    return ()

-- uses a fold to update the state machine in Model.hs
updateCreature  ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) => Creature ->
    Event t (Action) ->
    m (Dynamic t Creature)
updateCreature creature e =
  foldDyn ($) initCreature $ leftmost [
     -- creatureLifeCycle is our state machine function
     creatureLifeCycle <$> e
  ]

-- takes our state machine (the creature) to give us events that we feed back into the updateCreature fold
makeEvents ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Creature ->
  (Event t (), Event t (), Event t ()) ->
  m (Event t Action)
makeEvents dCreature topLevelButtonEvents = mdo
  -- timer to reset creature action after eating
  eTimeToResetActivity <- tickLossy 3 =<< liftIO getCurrentTime

  -- timer to make creature poop
  eTimeToScoop <- tickLossy 50 =<< liftIO getCurrentTime

  -- timer to make creature hungry
  eTimeToFeed <- tickLossy 30 =<< liftIO getCurrentTime

  -- timer to "evolve"
  eTimeToChangeStage <- tickLossy 10 =<< liftIO getCurrentTime

  -- timer to put creature to sleep
  eTimeToPutToSleep <- tickLossy 170 =<< liftIO getCurrentTime

  -- timer to wake creature up
  eTimeToWakeUp <- tickLossy 55 =<< liftIO getCurrentTime

  let 
      -- long food, short food, poop scoop
      (eFeedBanana, eFeedPlum, eScoop) = topLevelButtonEvents

      isHungry :: Appetite -> Bool
      isHungry appy =
        appy /= Full

      bAppetite = appetite <$> dCreature

      bIsHungry = isHungry <$> current bAppetite

      bNoPoop = not <$> bNeedToScoop

      bHungryAndNoPoop = (&&) <$> bIsHungry <*> bNoPoop

      -- sample value of creature activity after an update (TODO: think about "after")
      bActivity = creatureActivity <$> current dCreature
      
      isEating :: CreatureActivity -> Bool
      isEating act =
        act == Eating LongFood || act == Eating ShortFood

      bNeedToFrolick = isEating <$> bActivity

      bPoopState = poopState <$> current dCreature

      needToScoop :: PoopState -> Bool
      needToScoop ps =
        ps == PoopPresent

      bNeedToScoop = needToScoop <$> bPoopState

      -- event sets us back to frolicking after 4 seconds if we're not already frolicking. TODO: figure out other conditions.
      eChangeActivity = EventTriggerFrolick <$ gate bNeedToFrolick eTimeToResetActivity
      
      -- scoop the poop!
      ePoopEvent = UserScoopPoop <$ gate bNeedToScoop eScoop

      bStage = stage <$> current dCreature

      needToChangeStage :: Stage -> Bool
      needToChangeStage s =
        s == Egg

      bNeedToChangeStage = needToChangeStage <$> bStage

      eChangeStage = TimerChangeStage <$ gate bNeedToChangeStage eTimeToChangeStage

      -- feed creature whichever food user selects, can't select both at same time
      eFoodEvent = gate bHungryAndNoPoop $ leftmost [ UserFeedCreature LongFood <$ eFeedBanana, UserFeedCreature ShortFood <$ eFeedPlum ]

      -- scooping takes precedence over feeding which is further reflected in the state machine
      eUserEvent = leftmost [ ePoopEvent, eFoodEvent ]

      -- combine timer events, precedence left to right
      eTimerEvent = leftmost [ eChangeStage, TimerWakeUp <$ eTimeToWakeUp, TimerPutToSleep <$ eTimeToPutToSleep, TimerMakePoop <$ eTimeToScoop, TimerMakeHungry <$ eTimeToFeed, eChangeActivity ]

      -- combine timer and user events, precedence left to right
      eUserAndTimerEvents = leftmost [ eTimerEvent, eUserEvent ]
  return (eUserAndTimerEvents)

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
  m ()
determineView' stage consciousness poopState act appetite =
  case stage of
    Egg -> eggSVG
    Stage1 -> 
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
    Dead -> deadSVG
    StageEvil -> stageEvilSVG

-- main debug view
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

-- this function actually returns events along with generating DOM elements
topLevelButtons ::
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Dynamic t Creature -> 
  m (Event t (), Event t (), Event t ())
topLevelButtons dCreature = mdo
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