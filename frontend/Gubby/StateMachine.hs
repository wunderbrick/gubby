{-# LANGUAGE OverloadedStrings #-}

module StateMachine where

import Model

-- kill creature if n care mistakes
healthCheck :: Creature -> Creature
healthCheck creature =
    case creature of
        Creature stage consciousness poopState appetite foodJournal careMistakes creatureActivity -> 
            case Prelude.length careMistakes >= 6 of
                True -> Creature Dead consciousness poopState appetite foodJournal careMistakes creatureActivity 
                False -> Creature stage consciousness poopState appetite foodJournal careMistakes creatureActivity 

{-
Since we have two kinds of food (long items and short items) we have Morse code. Inputting FML in Morse code will turn the creature into
a Cthulhu-esque elder god that will destroy all reality.
FML in Morse code is:
F == short short long short
M == long long
L == short long short short

-}

-- if Morse code FML is entered, trigger evil mode and destroy all reality
evilCheck :: Creature -> Creature
evilCheck creature =
    case creature of
        Creature stage consciousness poopState appetite foodJournal careMistakes creatureActivity -> 
            case foodJournal of
              FoodJournal (Just ShortFood) (Just ShortFood) (Just LongFood) (Just ShortFood) (Just LongFood) (Just LongFood) (Just ShortFood) (Just LongFood) (Just ShortFood) (Just ShortFood) -> Creature StageEvil consciousness poopState appetite foodJournal careMistakes creatureActivity 
              _ -> Creature stage consciousness poopState appetite foodJournal careMistakes creatureActivity

-- update the state (decompose creature and call prime function to keep arguments simple)
creatureLifeCycle :: Action -> Creature -> Creature
creatureLifeCycle action creature =
    case creature of
        Creature stage consciousness poopState appetite foodJournal careMistakes creatureActivity ->
            evilCheck . healthCheck $ creatureLifeCycle' action stage consciousness poopState appetite foodJournal careMistakes creatureActivity  

creatureLifeCycle' :: Action -> Stage -> Consciousness -> PoopState -> Appetite -> FoodJournal -> CareMistakes -> CreatureActivity -> Creature

---------------------------
-- STAGE CHANGING EVENTS --
---------------------------

-- kill Gubby
creatureLifeCycle' action Dead consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    Dead 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

-- hatch Gubby
creatureLifeCycle' TimerChangeStage Egg consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    Stage1 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

-- so no other events other than TimerChangeStage affect Gubby while still an egg
creatureLifeCycle' action Egg consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    Egg 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

-- go full Lovecraft
creatureLifeCycle' action StageEvil consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    StageEvil 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

--------------------------
-- POOP EVENTS --
--------------------------

-- scoop poop while creature's awake (can't while asleep)
creatureLifeCycle' UserScoopPoop stage Awake PoopPresent appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    NoPoop 
    appetite 
    foodJournal 
    careMistakes 
    Frolicking

-- make Gubby poop while asleep, does not add care mistake
creatureLifeCycle' TimerMakePoop stage Asleep NoPoop appetite foodJournal careMistakes creatureActivity =
    Creature 
    stage 
    Asleep
    PoopPresent 
    appetite 
    foodJournal 
    careMistakes 
    AvoidingPoop

-- ignore PoopPresent while asleep (Gubby doesn't accumulate a care mistake)
creatureLifeCycle' TimerMakePoop stage Asleep PoopPresent appetite foodJournal careMistakes creatureActivity =
    Creature 
    stage 
    Asleep
    PoopPresent 
    appetite 
    foodJournal 
    careMistakes 
    AvoidingPoop

-- give Gubby a care mistake if awake and poop's present
creatureLifeCycle' TimerMakePoop stage Awake PoopPresent appetite foodJournal careMistakes creatureActivity =
    Creature 
    stage 
    Awake 
    PoopPresent 
    appetite 
    foodJournal 
    (careMistakes ++ [TooMuchPoop]) 
    AvoidingPoop

-- Gubby poops if it's time and there's no previous poop (no care mistake)
creatureLifeCycle' TimerMakePoop stage Awake NoPoop appetite foodJournal careMistakes creatureActivity =
    Creature 
    stage 
    Awake 
    PoopPresent 
    appetite 
    foodJournal 
    careMistakes
    AvoidingPoop

--------------------
-- FEEDING EVENTS --
--------------------

-- make Gubby hungry if full (no care mistake)
creatureLifeCycle' TimerMakeHungry stage consciousness poopStatus Full foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    consciousness 
    poopStatus 
    (Hungry (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)) 
    foodJournal 
    careMistakes 
    creatureActivity

-- if hunger timer goes off while Gubby's asleep don't add a care mistake
creatureLifeCycle' TimerMakeHungry stage Asleep poopStatus (Hungry stomach) foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Asleep 
    poopStatus 
    (Hungry stomach)
    foodJournal 
    careMistakes
    creatureActivity

-- don't add a care mistake for hunger if user's in the process of feeding Gubby when hunger timer goes off
creatureLifeCycle' TimerMakeHungry stage consciousness poopStatus (Hungry stomach) foodJournal careMistakes (Eating food) = 
    Creature 
    stage 
    consciousness 
    poopStatus 
    (Hungry stomach) 
    foodJournal 
    careMistakes
    (Eating food)

-- add care mistake if hunger timer goes off while Gubby's awake
creatureLifeCycle' TimerMakeHungry stage Awake poopStatus (Hungry stomach) foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    poopStatus 
    (Hungry stomach) 
    foodJournal 
    (careMistakes ++ [Hunger])
    creatureActivity

-- feed Gubby while awake and no poop present
creatureLifeCycle' (UserFeedCreature food) stage Awake NoPoop appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    NoPoop 
    (fillStomach food appetite) 
    (journalFood food foodJournal)  
    careMistakes 
    (Eating food)

--------------------------
-- CONSCIOUSNESS EVENTS --
--------------------------

-- put Gubby to sleep
creatureLifeCycle' TimerPutToSleep stage consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Asleep 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

-- wake Gubby up
creatureLifeCycle' TimerWakeUp stage consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

--------------------
-- FROLICK EVENTS --
--------------------

-- if it would be time to do a regular frolick when there's poop present, just keep the same poop present screen
creatureLifeCycle' EventTriggerFrolick stage consciousness poopState appetite foodJournal careMistakes AvoidingPoop = 
    Creature 
    stage 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    AvoidingPoop

-- make Gubby go back to frolicking (default state) if awake and no poop present
creatureLifeCycle' EventTriggerFrolick stage Awake poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    Frolicking

--------------------------
-- CATCH ALL, NO UPDATE --
--------------------------

creatureLifeCycle' action stage consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

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