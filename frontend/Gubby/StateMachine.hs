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
creatureLifeCycle' action Dead consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    Dead 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

creatureLifeCycle' TimerChangeStage Egg consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    Stage1 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

creatureLifeCycle' action Egg consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    Egg 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

creatureLifeCycle' action StageEvil consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    StageEvil 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

creatureLifeCycle' UserScoopPoop stage Awake PoopPresent appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    NoPoop 
    appetite 
    foodJournal 
    careMistakes 
    Frolicking

{-}
creatureLifeCycle' (UserFeedCreature food) stage Awake NoPoop Full foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    NoPoop 
    Full 
    foodJournal
    careMistakes 
    Frolicking
-}

creatureLifeCycle' (UserFeedCreature food) stage Awake NoPoop appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    NoPoop 
    (fillStomach food appetite) 
    (journalFood food foodJournal)  
    careMistakes 
    (Eating food)

creatureLifeCycle' TimerMakeHungry stage consciousness poopStatus Full foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    consciousness 
    poopStatus 
    (Hungry (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)) 
    foodJournal 
    careMistakes 
    creatureActivity

creatureLifeCycle' TimerMakeHungry stage Asleep poopStatus (Hungry stomach) foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Asleep 
    poopStatus 
    (Hungry stomach)
    foodJournal 
    careMistakes
    creatureActivity

creatureLifeCycle' TimerMakeHungry stage consciousness poopStatus (Hungry stomach) foodJournal careMistakes (Eating food) = 
    Creature 
    stage 
    consciousness 
    poopStatus 
    (Hungry stomach) 
    foodJournal 
    careMistakes -- don't add hunger to care mistake if feeding
    (Eating food)

creatureLifeCycle' TimerMakeHungry stage Awake poopStatus (Hungry stomach) foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    poopStatus 
    (Hungry stomach) 
    foodJournal 
    (careMistakes ++ [Hunger]) -- only add hunger to care mistakes while awake
    creatureActivity

creatureLifeCycle' TimerMakePoop stage Asleep NoPoop appetite foodJournal careMistakes creatureActivity =
    -- creature can poop itself while asleep but won't add a care mistake
    Creature 
    stage 
    Asleep
    PoopPresent 
    appetite 
    foodJournal 
    careMistakes 
    AvoidingPoop

creatureLifeCycle' TimerMakePoop stage Asleep PoopPresent appetite foodJournal careMistakes creatureActivity =
    -- creature can poop itself while asleep but won't add a care mistake
    Creature 
    stage 
    Asleep
    PoopPresent 
    appetite 
    foodJournal 
    careMistakes 
    AvoidingPoop

creatureLifeCycle' TimerMakePoop stage Awake PoopPresent appetite foodJournal careMistakes creatureActivity =
    -- only add poop that makes a care mistake when the creature's awake
    Creature 
    stage 
    Awake 
    PoopPresent 
    appetite 
    foodJournal 
    (careMistakes ++ [TooMuchPoop]) 
    AvoidingPoop

creatureLifeCycle' TimerMakePoop stage Awake NoPoop appetite foodJournal careMistakes creatureActivity =
    Creature 
    stage 
    Awake 
    PoopPresent 
    appetite 
    foodJournal 
    careMistakes
    AvoidingPoop

creatureLifeCycle' TimerPutToSleep stage consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Asleep 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

creatureLifeCycle' TimerWakeUp stage consciousness poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    creatureActivity

creatureLifeCycle' EventTriggerFrolick stage consciousness poopState appetite foodJournal careMistakes AvoidingPoop = 
    Creature 
    stage 
    consciousness 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    AvoidingPoop

creatureLifeCycle' EventTriggerFrolick stage Awake poopState appetite foodJournal careMistakes creatureActivity = 
    Creature 
    stage 
    Awake 
    poopState 
    appetite 
    foodJournal 
    careMistakes 
    Frolicking

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