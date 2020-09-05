module Model where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime)

data Food
    = LongFood
    | ShortFood
    deriving (Eq, Show)

type Stomach = (Maybe Food, Maybe Food, Maybe Food, Maybe Food, Maybe Food, Maybe Food)

data Appetite
    = Full
    | Hungry Stomach
    deriving (Eq, Show)

data PoopState
    = PoopPresent 
    | NoPoop
    deriving (Eq, Show)

data Consciousness
    = Awake
    | Asleep
    deriving (Eq, Show)

data Stage 
    = Egg
    | Stage1
    | StageEvil
    | Dead
    deriving (Eq, Show)

data FoodJournal =
    FoodJournal 
    (Maybe Food)
    (Maybe Food)
    (Maybe Food)
    (Maybe Food)
    (Maybe Food)
    (Maybe Food)
    (Maybe Food)
    (Maybe Food)
    (Maybe Food)
    (Maybe Food)
    deriving (Eq, Show)

goEvil :: FoodJournal
goEvil = 
    FoodJournal 
    -- F
    (Just ShortFood) 
    (Just ShortFood) 
    (Just LongFood) 
    (Just ShortFood) 
    -- M
    (Just LongFood) 
    (Just LongFood) 
    -- L
    (Just ShortFood) 
    (Just LongFood) 
    (Just ShortFood) 
    (Just ShortFood)

type CareMistakes = [CareMistake]

data CareMistake
    = Hunger
    | TooMuchPoop
    deriving (Eq, Show)

data Action
    = UserScoopPoop
    | UserFeedCreature Food
    | UserAdjustCareNeededFrequency NominalDiffTime
    | TimerMakeHungry
    | TimerMakePoop
    | TimerChangeStage
    | TimerPutToSleep
    | TimerWakeUp
    | EventTriggerFrolick
    deriving (Eq, Show)

data CreatureActivity
    = Frolicking
    | Eating Food
    | AvoidingPoop
    deriving (Eq, Show)

data Creature =
    Creature {
          stage :: Stage
        , consciousness :: Consciousness
        , poopState :: PoopState
        , appetite :: Appetite
        , foodJournal ::  FoodJournal-- the total history of what's been eaten, used to determine evil mode
        , careMistakes :: CareMistakes
        , creatureActivity :: CreatureActivity
    }
    deriving (Eq, Show)

initCreature :: Creature
initCreature =
    Creature 
    Egg 
    Awake 
    NoPoop
    Full
    (FoodJournal Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
    []
    Frolicking
    