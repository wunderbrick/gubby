{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module SVG where

import Control.Monad.Fix (MonadFix)
import Data.Text (pack, unpack, Text)
import Data.Map

import Reflex
import Reflex.Dom.Core

import Model
import StateMachine

-- TODO: Refactor this atrocious mess
-- https://stackoverflow.com/questions/38268962/a-single-svg-element-with-reflex-frp
eggSVG :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  m ()
eggSVG = do
  let 
    svgAttrs :: Reflex t => Dynamic t (Map Text Text)
    svgAttrs = constDyn $ fromList
                [ ("width" , "200")
                , ("height" , "200")
                ]

    eggAttrs :: Reflex t => Dynamic t (Map Text Text)
    eggAttrs = constDyn $ fromList
                [ ("cx", "80")
                , ("cy", "80")
                , ("rx", "30")
                , ("ry", "40")
                , ("stroke", "black")
                , ("stroke-width", "1.5")
                , ("style", "fill:purple" )
                ]

    animateEggAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateEggAttrs = constDyn $ fromList
                [ ("attributeName", "rx")
                , ("values", "30;32;30")
                , ("dur", "1s")
                , ("repeatCount", "indefinite")
                ]

    animateEggAttrs' :: Reflex t => Dynamic t (Map Text Text)
    animateEggAttrs' = constDyn $ fromList
                [ ("attributeName", "ry")
                , ("values", "40;36;40")
                , ("dur", "1s")
                , ("repeatCount", "indefinite")
                ]

    shadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    shadowAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "135")
               , ("rx", "30")
               , ("ry", "5")
               , ("style", "fill:black")
               ]
    
    animateShadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateShadowAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "30;32;30")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    eggSpotAttrs :: Reflex t => Dynamic t (Map Text Text)
    eggSpotAttrs = constDyn $ fromList
                [ ("cx", "90")
                , ("cy", "100")
                , ("rx", "3")
                , ("ry", "4")
                , ("style", "fill:white" )
                ]

    animateEggSpotAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateEggSpotAttrs = constDyn $ fromList
                [ ("attributeName", "rx")
               , ("values", "3;4;3")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    animateEggSpotAttrs' :: Reflex t => Dynamic t (Map Text Text)
    animateEggSpotAttrs' = constDyn $ fromList
                [ ("attributeName", "yx")
               , ("values", "4;3;4")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    svgTag = elSvg "svg" svgAttrs eggTag

    eggTag = 
      el3Svg 
      "ellipse" 
      eggAttrs
      animateEggTag
      "ellipse" 
      shadowAttrs 
      animateShadowTag
      "ellipse"
      eggSpotAttrs
      animateEggSpotTag

    animateEggTag = el2Svg "animate" animateEggAttrs (return ()) "animate" animateEggAttrs' (return ())
    animateEggSpotTag = el2Svg "animate" animateEggSpotAttrs (return ()) "animate" animateEggSpotAttrs' (return ())
    animateShadowTag = elSvg "animate" animateShadowAttrs $ return ()
      
  drawSvg <- svgTag

  return ()

frolickingSVG :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  m ()
frolickingSVG = do
  let 
    svgAttrs :: Reflex t => Dynamic t (Map Text Text)
    svgAttrs = constDyn $ fromList
               [ ("width" , "200")
               , ("height" , "200")
               ]
  
    bodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    bodyAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "80")
               , ("r", "40")
               , ("stroke", "green")
               , ("stroke-width", "3")
               , ("fill",  "yellow" )
               ]

    animateBodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateBodyAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    outerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    outerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("r", "10")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "white" )
               ]

    animateOuterEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateOuterEyeAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "10;15;10")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    innerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    innerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("r", "5")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "black" )
               ]

    animateInnerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateInnerEyeAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "5;7;5")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    mouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    mouthAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "100")
               , ("rx", "12")
               , ("ry", "3")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("style", "fill:red")
               ]

    animateMouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "8;14;8")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    animateMouthAttrs' :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs' = constDyn $ fromList
               [ ("attributeName", "cy")
               , ("values", "100;105;100")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    shadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    shadowAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "135")
               , ("rx", "45")
               , ("ry", "5")
               , ("style", "fill:black")
               ]
    
    animateShadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateShadowAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    -- recursive do lets us put these in the order they appear in the DOM
    svgTag = elSvg "svg" svgAttrs creatureTag
    
    creatureTag = 
      el5Svg 
      "circle" 
      bodyAttrs 
      animateBodyTag 
      "circle"
      outerEyeAttrs
      animateOuterEyeTag
      "circle"
      innerEyeAttrs
      animateInnerEyeTag
      "ellipse"
      mouthAttrs
      animateMouthTag
      "ellipse" 
      shadowAttrs 
      animateShadowTag
    
    animateBodyTag = elSvg "animate" animateBodyAttrs $ return ()
    animateOuterEyeTag = elSvg "animate" animateOuterEyeAttrs $ return ()
    animateInnerEyeTag = elSvg "animate" animateInnerEyeAttrs $ return ()
    animateMouthTag = el2Svg "animate" animateMouthAttrs (return ()) "animate" animateMouthAttrs' (return ())
    animateShadowTag = elSvg "animate" animateShadowAttrs $ return ()

  drawSvg <- svgTag
  return ()

hungryFrolickingSVG :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  m ()
hungryFrolickingSVG = do
  let 
    svgAttrs :: Reflex t => Dynamic t (Map Text Text)
    svgAttrs = constDyn $ fromList
               [ ("width" , "200")
               , ("height" , "200")
               ]
  
    bodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    bodyAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "80")
               , ("r", "40")
               , ("stroke", "green")
               , ("stroke-width", "3")
               , ("fill",  "yellow" )
               ]

    animateBodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateBodyAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    outerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    outerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("r", "10")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "white" )
               ]

    animateOuterEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateOuterEyeAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "10;15;10")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    innerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    innerEyeAttrs = constDyn $ fromList
               [ ("cx", "80.0")
               , ("cy", "75.0")
               , ("r", "5")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "black" )
               ]

    animateInnerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateInnerEyeAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "5;7;5")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    mouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    mouthAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "100")
               , ("rx", "9")
               , ("ry", "6")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("style", "fill:red")
               ]

    animateMouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "9;11;9")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    hungerBubbleAttrs :: Reflex t => Dynamic t (Map Text Text)
    hungerBubbleAttrs = constDyn $ fromList
               [ ("cx", "150")
               , ("cy", "25")
               , ("rx", "32")
               , ("ry", "18")
               , ("stroke", "black")
               , ("stroke-width", "1.5")
               , ("style", "fill:white")
               ]

    hungerBubbleTextAttrs :: Reflex t => Dynamic t (Map Text Text)
    hungerBubbleTextAttrs = constDyn $ fromList
               [ ("x", "136")
               , ("y", "36")
               , ("font-size", "2em")
               ]

    animateMouthAttrs' :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs' = constDyn $ fromList
               [ ("attributeName", "cy")
               , ("values", "100;105;100")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    shadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    shadowAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "135")
               , ("rx", "45")
               , ("ry", "5")
               , ("style", "fill:black")
               ]
    
    animateShadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateShadowAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    -- recursive do lets us put these in the order they appear in the DOM
    svgTag = elSvg "svg" svgAttrs creatureTag
    
    creatureTag = 
      el7Svg 
      "circle" 
      bodyAttrs 
      animateBodyTag 
      "circle"
      outerEyeAttrs
      animateOuterEyeTag
      "circle"
      innerEyeAttrs
      animateInnerEyeTag
      "ellipse"
      mouthAttrs
      animateMouthTag
      "ellipse"
      hungerBubbleAttrs
      (return ())
      "text"
      hungerBubbleTextAttrs
      (text "H!")
      "ellipse" 
      shadowAttrs 
      animateShadowTag
    
    animateBodyTag = elSvg "animate" animateBodyAttrs $ return ()
    animateOuterEyeTag = elSvg "animate" animateOuterEyeAttrs $ return ()
    animateInnerEyeTag = elSvg "animate" animateInnerEyeAttrs $ return ()
    animateMouthTag = el2Svg "animate" animateMouthAttrs (return ()) "animate" animateMouthAttrs' (return ())
    --animateMouthTag' = elSvg "animate"  animateMouthAttrs' $ return ()
    animateShadowTag = elSvg "animate" animateShadowAttrs $ return ()

  drawSvg <- svgTag
  return ()

avoidingPoopSVG :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  m ()
avoidingPoopSVG = do
  let 
    svgAttrs :: Reflex t => Dynamic t (Map Text Text)
    svgAttrs = constDyn $ fromList
               [ ("width" , "200")
               , ("height" , "200")
               ]
  
    bodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    bodyAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "80")
               , ("r", "40")
               , ("stroke", "green")
               , ("stroke-width", "3")
               , ("fill",  "yellow" )
               ]

    animateBodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateBodyAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    outerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    outerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("r", "10")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "white" )
               ]

    animateOuterEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateOuterEyeAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "10;15;10")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    innerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    innerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("r", "5")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "black" )
               ]

    animateInnerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateInnerEyeAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "5;7;5")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    mouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    mouthAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "100")
               , ("rx", "12")
               , ("ry", "3")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("style", "fill:red")
               ]

    animateMouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "8;14;8")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    animateMouthAttrs' :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs' = constDyn $ fromList
               [ ("attributeName", "cy")
               , ("values", "100;105;100")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    poopAttrs :: Reflex t => Dynamic t (Map Text Text)
    poopAttrs = constDyn $ fromList
               [ ("cx", "120")
               , ("cy", "128")
               , ("rx", "14")
               , ("ry", "10")
               , ("stroke", "black")
               , ("stroke-width", "1.5")
               , ("style", "fill:brown")
               ]

    poopShadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    poopShadowAttrs = constDyn $ fromList
               [ ("cx", "120")
               , ("cy", "138")
               , ("rx", "14")
               , ("ry", "2")
               , ("style", "fill:black")
               ]

    fly1Attrs :: Reflex t => Dynamic t (Map Text Text)
    fly1Attrs = constDyn $ fromList
               [ ("cx", "141")
               , ("cy", "116")
               , ("rx", "1.5")
               , ("ry", "1.5")
               , ("style", "fill:black")
               ]

    fly2Attrs :: Reflex t => Dynamic t (Map Text Text)
    fly2Attrs = constDyn $ fromList
               [ ("cx", "141")
               , ("cy", "110")
               , ("rx", "1.5")
               , ("ry", "1.5")
               , ("style", "fill:black")
               ]

    animateFly1Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateFly1Attrs = constDyn $ fromList
               [ ("attributeName", "cy")
               , ("values", "116;120;116")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    animateFly2Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateFly2Attrs = constDyn $ fromList
               [ ("attributeName", "cx")
               , ("values", "110;115;110")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    shadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    shadowAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "135")
               , ("rx", "45")
               , ("ry", "5")
               , ("style", "fill:black")
               ]
    
    animateShadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateShadowAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    -- recursive do lets us put these in the order they appear in the DOM
    svgTag = elSvg "svg" svgAttrs creatureTag
    
    creatureTag = 
      el9Svg 
      "circle" 
      bodyAttrs 
      animateBodyTag 
      "circle"
      outerEyeAttrs
      animateOuterEyeTag
      "circle"
      innerEyeAttrs
      animateInnerEyeTag
      "ellipse"
      mouthAttrs
      animateMouthTag
      "ellipse" 
      shadowAttrs 
      animateShadowTag
      "ellipse"
      poopAttrs
      (return ())
      "ellipse"
      fly1Attrs
      animateFly1Tag
      "ellipse"
      fly2Attrs
      animateFly2Tag
      "ellipse"
      poopShadowAttrs
      (return ())
    
    animateBodyTag = elSvg "animate" animateBodyAttrs $ return ()
    animateOuterEyeTag = elSvg "animate" animateOuterEyeAttrs $ return ()
    animateInnerEyeTag = elSvg "animate" animateInnerEyeAttrs $ return ()
    animateMouthTag = el2Svg "animate" animateMouthAttrs (return ()) "animate" animateMouthAttrs' (return ())
    animateFly1Tag = elSvg "animate" animateFly1Attrs $ return ()
    animateFly2Tag = elSvg "animate" animateFly2Attrs $ return ()
    animateShadowTag = elSvg "animate" animateShadowAttrs $ return ()

  drawSvg <- svgTag
  return ()

sleepingSVG :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  m ()
sleepingSVG = do
  let 
    svgAttrs :: Reflex t => Dynamic t (Map Text Text)
    svgAttrs = constDyn $ fromList
               [ ("width" , "200")
               , ("height" , "200")
               ]
  
    bodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    bodyAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "80")
               , ("r", "40")
               , ("stroke", "green")
               , ("stroke-width", "3")
               , ("fill",  "yellow" )
               ]

    animateBodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateBodyAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    outerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    outerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("rx", "10")
               , ("ry", "2")
               , ("style",  "fill:black" )
               ]

    animateOuterEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateOuterEyeAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "10;12;10")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    mouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    mouthAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "100")
               , ("rx", "12")
               , ("ry", "3")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("style", "fill:red")
               ]

    animateMouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "8;14;8")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    animateMouthAttrs' :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs' = constDyn $ fromList
               [ ("attributeName", "cy")
               , ("values", "100;105;100")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    sleepBubbleAttrs :: Reflex t => Dynamic t (Map Text Text)
    sleepBubbleAttrs = constDyn $ fromList
               [ ("cx", "150")
               , ("cy", "25")
               , ("rx", "32")
               , ("ry", "18")
               , ("stroke", "black")
               , ("stroke-width", "1.5")
               , ("style", "fill:white")
               ]

    sleepBubbleTextAttrs :: Reflex t => Dynamic t (Map Text Text)
    sleepBubbleTextAttrs = constDyn $ fromList
               [ ("x", "136")
               , ("y", "36")
               , ("font-size", "2em")
               ]

    shadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    shadowAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "135")
               , ("rx", "45")
               , ("ry", "5")
               , ("style", "fill:black")
               ]
    
    animateShadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateShadowAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    -- recursive do lets us put these in the order they appear in the DOM
    svgTag = elSvg "svg" svgAttrs creatureTag
    
    creatureTag = 
      el6Svg 
      "circle" 
      bodyAttrs 
      animateBodyTag 
      "ellipse"
      outerEyeAttrs
      animateOuterEyeTag
      "ellipse"
      mouthAttrs
      animateMouthTag
      "ellipse"
      sleepBubbleAttrs
      (return ())
      "text"
      sleepBubbleTextAttrs
      (text "Z.")
      "ellipse" 
      shadowAttrs 
      animateShadowTag
    
    animateBodyTag = elSvg "animate" animateBodyAttrs $ return ()
    animateOuterEyeTag = elSvg "animate" animateOuterEyeAttrs $ return ()
    animateMouthTag = el2Svg "animate" animateMouthAttrs (return ()) "animate" animateMouthAttrs' (return ())
    animateShadowTag = elSvg "animate" animateShadowAttrs $ return ()

  drawSvg <- svgTag
  return ()

sleepingNextToPoopSVG :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  m ()
sleepingNextToPoopSVG = do
  let 
    svgAttrs :: Reflex t => Dynamic t (Map Text Text)
    svgAttrs = constDyn $ fromList
               [ ("width" , "200")
               , ("height" , "200")
               ]
  
    bodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    bodyAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "80")
               , ("r", "40")
               , ("stroke", "green")
               , ("stroke-width", "3")
               , ("fill",  "yellow" )
               ]

    animateBodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateBodyAttrs = constDyn $ fromList
               [ ("attributeName", "r")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    outerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    outerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("rx", "10")
               , ("ry", "2")
               , ("style",  "fill:black" )
               ]

    animateOuterEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateOuterEyeAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "10;12;10")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    mouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    mouthAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "100")
               , ("rx", "12")
               , ("ry", "3")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("style", "fill:red")
               ]

    animateMouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "8;14;8")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    animateMouthAttrs' :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs' = constDyn $ fromList
               [ ("attributeName", "cy")
               , ("values", "100;105;100")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    sleepBubbleAttrs :: Reflex t => Dynamic t (Map Text Text)
    sleepBubbleAttrs = constDyn $ fromList
               [ ("cx", "150")
               , ("cy", "25")
               , ("rx", "32")
               , ("ry", "18")
               , ("stroke", "black")
               , ("stroke-width", "1.5")
               , ("style", "fill:white")
               ]

    sleepBubbleTextAttrs :: Reflex t => Dynamic t (Map Text Text)
    sleepBubbleTextAttrs = constDyn $ fromList
               [ ("x", "136")
               , ("y", "36")
               , ("font-size", "2em")
               ]

    poopAttrs :: Reflex t => Dynamic t (Map Text Text)
    poopAttrs = constDyn $ fromList
               [ ("cx", "120")
               , ("cy", "128")
               , ("rx", "14")
               , ("ry", "10")
               , ("stroke", "black")
               , ("stroke-width", "1.5")
               , ("style", "fill:brown")
               ]

    poopShadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    poopShadowAttrs = constDyn $ fromList
               [ ("cx", "120")
               , ("cy", "138")
               , ("rx", "14")
               , ("ry", "2")
               , ("style", "fill:black")
               ]

    fly1Attrs :: Reflex t => Dynamic t (Map Text Text)
    fly1Attrs = constDyn $ fromList
               [ ("cx", "141")
               , ("cy", "116")
               , ("rx", "1.5")
               , ("ry", "1.5")
               , ("style", "fill:black")
               ]

    fly2Attrs :: Reflex t => Dynamic t (Map Text Text)
    fly2Attrs = constDyn $ fromList
               [ ("cx", "141")
               , ("cy", "110")
               , ("rx", "1.5")
               , ("ry", "1.5")
               , ("style", "fill:black")
               ]

    animateFly1Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateFly1Attrs = constDyn $ fromList
               [ ("attributeName", "cy")
               , ("values", "116;120;116")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    animateFly2Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateFly2Attrs = constDyn $ fromList
               [ ("attributeName", "cx")
               , ("values", "110;115;110")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    shadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    shadowAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "135")
               , ("rx", "45")
               , ("ry", "5")
               , ("style", "fill:black")
               ]
    
    animateShadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateShadowAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "40;50;40")
               , ("dur", "4s")
               , ("repeatCount", "indefinite")
               ]

    -- recursive do lets us put these in the order they appear in the DOM
    svgTag = elSvg "svg" svgAttrs creatureTag
    
    creatureTag = 
      el10Svg 
      "circle" 
      bodyAttrs 
      animateBodyTag 
      "ellipse"
      outerEyeAttrs
      animateOuterEyeTag
      "ellipse"
      mouthAttrs
      animateMouthTag
      "ellipse"
      sleepBubbleAttrs
      (return ())
      "text"
      sleepBubbleTextAttrs
      (text "Z.")
      "ellipse" 
      shadowAttrs 
      animateShadowTag
      "ellipse"
      poopAttrs
      (return ())
      "ellipse"
      fly1Attrs
      animateFly1Tag
      "ellipse"
      fly2Attrs
      animateFly2Tag
      "ellipse"
      poopShadowAttrs
      (return ())
    
    animateBodyTag = elSvg "animate" animateBodyAttrs $ return ()
    animateOuterEyeTag = elSvg "animate" animateOuterEyeAttrs $ return ()
    animateMouthTag = el2Svg "animate" animateMouthAttrs (return ()) "animate" animateMouthAttrs' (return ())
    animateFly1Tag = elSvg "animate" animateFly1Attrs $ return ()
    animateFly2Tag = elSvg "animate" animateFly2Attrs $ return ()
    animateShadowTag = elSvg "animate" animateShadowAttrs $ return ()

  drawSvg <- svgTag
  return ()

deadSVG :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  m ()
deadSVG = do
  let 
    svgAttrs :: Reflex t => Dynamic t (Map Text Text)
    svgAttrs = constDyn $ fromList
               [ ("width" , "200")
               , ("height" , "200")
               ]
  
    bodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    bodyAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "126")
               , ("rx", "45")
               , ("ry", "10")
               , ("stroke", "green")
               , ("stroke-width", "3")
               , ("fill",  "yellow" )
               ]

    eyeTextAttrs :: Reflex t => Dynamic t (Map Text Text)
    eyeTextAttrs = constDyn $ fromList
               [ ("x", "85")
               , ("y", "129")
               , ("font-size", "1em")
               , ("font-family", "monospace")
               , ("font-weight", "bold")
               ]

    fly1Attrs :: Reflex t => Dynamic t (Map Text Text)
    fly1Attrs = constDyn $ fromList
               [ ("cx", "106")
               , ("cy", "106")
               , ("rx", "1.5")
               , ("ry", "1.5")
               , ("style", "fill:black")
               ]

    fly2Attrs :: Reflex t => Dynamic t (Map Text Text)
    fly2Attrs = constDyn $ fromList
               [ ("cx", "125")
               , ("cy", "100")
               , ("rx", "1.5")
               , ("ry", "1.5")
               , ("style", "fill:black")
               ]

    fly3Attrs :: Reflex t => Dynamic t (Map Text Text)
    fly3Attrs = constDyn $ fromList
               [ ("cx", "55")
               , ("cy", "75")
               , ("rx", "1.5")
               , ("ry", "1.5")
               , ("style", "fill:black")
               ]

    animateFly1Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateFly1Attrs = constDyn $ fromList
               [ ("attributeName", "cy")
               , ("values", "106;110;106")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    animateFly2Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateFly2Attrs = constDyn $ fromList
               [ ("attributeName", "cx")
               , ("values", "100;105;100")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    animateFly3Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateFly3Attrs = constDyn $ fromList
               [ ("attributeName", "cx")
               , ("values", "55;50;55")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    shadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    shadowAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "135")
               , ("rx", "45")
               , ("ry", "4")
               , ("style", "fill:black")
               ]

    -- recursive do lets us put these in the order they appear in the DOM
    svgTag = elSvg "svg" svgAttrs corpseTag
    
    corpseTag = 
      el6Svg 
      "ellipse" 
      bodyAttrs 
      (return ())
      "text"
      eyeTextAttrs
      (text "X") 
      "ellipse"
      fly1Attrs
      animateFly1Tag
      "ellipse"
      fly2Attrs
      animateFly2Tag
      "ellipse"
      fly3Attrs
      animateFly3Tag
      "ellipse"
      shadowAttrs
      (return ())
      
    animateFly1Tag = elSvg "animate" animateFly1Attrs $ return ()
    animateFly2Tag = elSvg "animate" animateFly2Attrs $ return ()
    animateFly3Tag = elSvg "animate" animateFly3Attrs $ return ()

  drawSvg <- svgTag

  return ()

eatingSVG :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) => 
  Food ->
  m ()
eatingSVG food = do
  let 
    svgAttrs :: Reflex t => Dynamic t (Map Text Text)
    svgAttrs = constDyn $ fromList
               [ ("width" , "200")
               , ("height" , "200")
               ]
  
    bodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    bodyAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "80")
               , ("r", "50")
               , ("stroke", "green")
               , ("stroke-width", "3")
               , ("fill",  "yellow" )
               ]

    outerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    outerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("r", "15")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "white" )
               ]

    innerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    innerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("r", "7")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "black" )
               ]

    foodAttrs :: Reflex t => Food -> Dynamic t (Map Text Text)
    foodAttrs food = 
      case food of
        ShortFood ->
          constDyn $ fromList
                  [ ("cx", "105")
                  , ("cy", "98")
                  , ("r", "7")
                  , ("stroke", "black")
                  , ("stroke-width", "1")
                  , ("fill",  "purple" )
                  ]

        LongFood ->
          constDyn $ fromList
                  [ ("cx",  "110")
                  , ("cy", "98")
                  , ("rx", "14")
                  , ("ry", "6")
                  , ("stroke", "black")
                  , ("stroke-width", "1")
                  , ("fill",  "yellow" )
                  ]

    animateFoodAttrs :: Reflex t => Food -> Dynamic t (Map Text Text)
    animateFoodAttrs food = 
      case food of
        ShortFood ->
          constDyn $ fromList
               [ ("attributeName", "cx")
               , ("values", "105;100;100")
               , ("dur", "1s")
               , ("repeatCount", "1")
               ]


        LongFood ->
          constDyn $ fromList
               [ ("attributeName", "cx")
               , ("values", "110;105;105")
               , ("dur", "1s")
               , ("repeatCount", "1")
               ]

    mouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    mouthAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "100")
               , ("rx", "12")
               , ("ry", "3")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("style", "fill:red")
               ]

    animateMouthAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs = constDyn $ fromList
               [ ("attributeName", "rx")
               , ("values", "12;13;12")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    animateMouthAttrs' :: Reflex t => Dynamic t (Map Text Text)
    animateMouthAttrs' = constDyn $ fromList
               [ ("attributeName", "ry")
               , ("values", "3;7;3")
               , ("dur", "1s")
               , ("repeatCount", "indefinite")
               ]

    shadowAttrs :: Reflex t => Dynamic t (Map Text Text)
    shadowAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "135")
               , ("rx", "45")
               , ("ry", "5")
               , ("style", "fill:black")
               ]

    -- recursive do lets us put these in the order they appear in the DOM
    svgTag = elSvg "svg" svgAttrs creatureTag
    
    foodText = (if food == ShortFood then "circle" else "ellipse")

    creatureTag = 
      el6Svg 
      "circle" 
      bodyAttrs 
      (return ()) 
      "circle"
      outerEyeAttrs
      (return ())
      "circle"
      innerEyeAttrs
      (return ())
      "ellipse"
      mouthAttrs
      animateMouthTag
      foodText
      (foodAttrs food)
      animateFoodTag
      "ellipse" 
      shadowAttrs 
      (return ())
    
    animateMouthTag = el2Svg "animate" animateMouthAttrs (return ()) "animate" animateMouthAttrs' (return ())
    animateFoodTag = elSvg "animate" (animateFoodAttrs food) (return ())

  drawSvg <- svgTag
  return ()

stageEvilSVG :: 
  ( MonadWidget t m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  m ()
stageEvilSVG = do
  let 
    svgAttrs :: Reflex t => Dynamic t (Map Text Text)
    svgAttrs = constDyn $ fromList
               [ ("width" , "200")
               , ("height" , "200")
               ]

    wingsAttrs :: Reflex t => Dynamic t (Map Text Text)
    wingsAttrs = constDyn $ fromList
               [ ("d" , "M 12 140 L 1 90 C 81 120 82 120 161 90 L 146 140 C 81 130 82 130 12 140")
               , ("height" , "200")
               , ("stroke", "black")
               , ("stroke-width", "3")
               , ("fill",  "#00cc66")
               ]

    animateWingsAttrs :: Reflex t => Dynamic t (Map Text Text)
    animateWingsAttrs = constDyn $ fromList
                [ ("attributeName", "d")
                , ("values", "M 12 140 L 1 90 C 81 120 82 120 161 90 L 146 140 C 81 130 82 130 12 140;M 12 146 L 1 96 C 81 126 82 126 161 96 L 146 146 C 81 136 82 136 12 146;M 12 140 L 1 90 C 81 120 82 120 161 90 L 146 140 C 81 130 82 130 12 140")
                , ("dur", "1s")
                , ("repeatCount", "indefinite")
                ]

    bodyAttrs :: Reflex t => Dynamic t (Map Text Text)
    bodyAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "80")
               , ("r", "68")
               , ("stroke", "black")
               , ("stroke-width", "3")
               , ("fill",  "#00cc66")
               ]

    outerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    outerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("r", "25")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "white" )
               ]

    innerEyeAttrs :: Reflex t => Dynamic t (Map Text Text)
    innerEyeAttrs = constDyn $ fromList
               [ ("cx", "80")
               , ("cy", "75")
               , ("r", "14")
               , ("stroke", "green")
               , ("stroke-width", "1")
               , ("fill",  "black" )
               ]

    -- 1-4 is L-R

    tentacle1Attr :: Reflex t => Dynamic t (Map Text Text)
    tentacle1Attr = constDyn $ fromList
               [ ("d", "M 30 115 C 37 165 43 160 50 120")
               , ("stroke-width", "4")
               , ("stroke", "black")
               , ("fill",  "#00cc66")
               ]

    animateTentacle1Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateTentacle1Attrs = constDyn $ fromList
                [ ("attributeName", "d")
                , ("values", "M 30 115 C 37 165 43 160 50 120;M 30 115 C 34 165 40 160 50 120;M 30 115 C 37 165 43 160 50 120")
                , ("dur", "1s")
                , ("repeatCount", "indefinite")
                ]

    tentacle2Attr :: Reflex t => Dynamic t (Map Text Text)
    tentacle2Attr = constDyn $ fromList
               [ ("d", "M 55 122 C 62 168 68 163 75 123")
               , ("stroke-width", "4")
               , ("stroke", "black")
               , ("fill",  "#00cc66")
               ] 

    animateTentacle2Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateTentacle2Attrs = constDyn $ fromList
                [ ("attributeName", "d")
                , ("values", "M 55 122 C 62 168 68 163 75 123;M 55 122 C 65 168 72 163 75 123;M 55 122 C 62 168 68 163 75 123")
                , ("dur", "1s")
                , ("repeatCount", "indefinite")
                ]

    tentacle3Attr :: Reflex t => Dynamic t (Map Text Text)
    tentacle3Attr = constDyn $ fromList
               [ ("d", "M 107 122 C 100 168 99 163 87 123")
               , ("stroke-width", "4")
               , ("stroke", "black")
               , ("fill",  "#00cc66")
               ] 

    animateTentacle3Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateTentacle3Attrs = constDyn $ fromList
                [ ("attributeName", "d")
                , ("values", "M 107 122 C 100 168 99 163 87 123;M 107 122 C 97 168 96 163 87 123;M 107 122 C 100 168 99 163 87 123")
                , ("dur", "1s")
                , ("repeatCount", "indefinite")
                ]

    tentacle4Attr :: Reflex t => Dynamic t (Map Text Text)
    tentacle4Attr = constDyn $ fromList
               [ ("d", "M 132 115 C 125 165 119 160 112 120")
               , ("stroke-width", "4")
               , ("stroke", "black")
               , ("fill",  "#00cc66")
               ]

    animateTentacle4Attrs :: Reflex t => Dynamic t (Map Text Text)
    animateTentacle4Attrs = constDyn $ fromList
                [ ("attributeName", "d")
                , ("values", "M 132 115 C 125 165 119 160 112 120;M 132 115 C 125 165 122 160 112 120;M 132 115 C 125 165 119 160 112 120")
                , ("dur", "1s")
                , ("repeatCount", "indefinite")
                ]
    
    svgTag = elSvg "svg" svgAttrs creatureTag

    creatureTag =
      el8Svg
      "path"
      wingsAttrs
      animateWingsTag
      "circle"
      bodyAttrs
      (return ())
      "circle"
      outerEyeAttrs
      (return ())
      "circle"
      innerEyeAttrs
      (return ())
      "path"
      tentacle1Attr
      animateTentacle1Tag
      "path"
      tentacle2Attr
      animateTentacle2Tag
      "path"
      tentacle3Attr
      animateTentacle3Tag
      "path"
      tentacle4Attr
      animateTentacle4Tag

    animateWingsTag = elSvg "animate" animateWingsAttrs (return ())
    animateTentacle1Tag = elSvg "animate" animateTentacle1Attrs (return ())
    animateTentacle2Tag = elSvg "animate" animateTentacle2Attrs (return ())
    animateTentacle3Tag = elSvg "animate" animateTentacle3Attrs (return ())
    animateTentacle4Tag = elSvg "animate" animateTentacle4Attrs (return ())


  drawSvg <- svgTag

  el "br" blank
  text "An elder god was lurking inside your creature! You've released it on your reality!"
  el "br" blank
  text "Time and space lose all meaning and you descend into the madness of the void..."
  el "br" blank
  text ("(You fed your creature a cursed combination of food, \"FML\" in Morse code!")
  el "br" blank
  text "Even the uninitiated know this is a deadly combination in the Cultist's Cookbook..."

  return ()

elSvg :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m a -> m ()
elSvg tag a1 a2 = do
  elDynAttrNS' ns tag a1 a2
  return ()

el2Svg :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m a -> Text -> Dynamic t (Map Text Text) -> m a -> m ()
el2Svg tag1 a1 a2 tag2 a3 a4 = do
  elDynAttrNS' ns tag1 a1 a2
  elDynAttrNS' ns tag2 a3 a4
  return ()

el3Svg :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m a -> Text -> Dynamic t (Map Text Text) -> m a -> Text -> Dynamic t (Map Text Text) -> m a -> m ()
el3Svg tag1 a1 a2 tag2 a3 a4 tag3 a5 a6 = do
  elDynAttrNS' ns tag1 a1 a2
  elDynAttrNS' ns tag2 a3 a4
  elDynAttrNS' ns tag3 a5 a6
  return ()

el4Svg :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m a -> Text -> Dynamic t (Map Text Text) -> m a -> Text -> Dynamic t (Map Text Text) -> m a -> Text -> Dynamic t (Map Text Text) -> m a -> m ()
el4Svg tag1 a1 a2 tag2 a3 a4 tag3 a5 a6 tag4 a7 a8 = do
  elDynAttrNS' ns tag1 a1 a2
  elDynAttrNS' ns tag2 a3 a4
  elDynAttrNS' ns tag3 a5 a6
  elDynAttrNS' ns tag4 a7 a8
  return ()

el5Svg :: 
  (DomBuilder t m, PostBuild t m) => 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) ->
  m a -> 
  Text ->
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->  
  m ()
el5Svg tag1 a1 a2 tag2 a3 a4 tag3 a5 a6 tag4 a7 a8 tag5 a9 a10 = do
  elDynAttrNS' ns tag1 a1 a2
  elDynAttrNS' ns tag2 a3 a4
  elDynAttrNS' ns tag3 a5 a6
  elDynAttrNS' ns tag4 a7 a8
  elDynAttrNS' ns tag5 a9 a10
  return ()

el6Svg :: 
  (DomBuilder t m, PostBuild t m) => 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) ->
  m a -> 
  Text ->
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->  
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  m ()
el6Svg tag1 a1 a2 tag2 a3 a4 tag3 a5 a6 tag4 a7 a8 tag5 a9 a10 tag6 a11 a12 = do
  elDynAttrNS' ns tag1 a1 a2
  elDynAttrNS' ns tag2 a3 a4
  elDynAttrNS' ns tag3 a5 a6
  elDynAttrNS' ns tag4 a7 a8
  elDynAttrNS' ns tag5 a9 a10
  elDynAttrNS' ns tag6 a11 a12
  return ()

el7Svg :: 
  (DomBuilder t m, PostBuild t m) => 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) ->
  m a -> 
  Text ->
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->  
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  m ()
el7Svg tag1 a1 a2 tag2 a3 a4 tag3 a5 a6 tag4 a7 a8 tag5 a9 a10 tag6 a11 a12 tag7 a13 a14 = do
  elDynAttrNS' ns tag1 a1 a2
  elDynAttrNS' ns tag2 a3 a4
  elDynAttrNS' ns tag3 a5 a6
  elDynAttrNS' ns tag4 a7 a8
  elDynAttrNS' ns tag5 a9 a10
  elDynAttrNS' ns tag6 a11 a12
  elDynAttrNS' ns tag7 a13 a14
  return ()

el8Svg :: 
  (DomBuilder t m, PostBuild t m) => 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) ->
  m a -> 
  Text ->
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->  
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  m ()
el8Svg tag1 a1 a2 tag2 a3 a4 tag3 a5 a6 tag4 a7 a8 tag5 a9 a10 tag6 a11 a12 tag7 a13 a14 tag8 a15 a16 = do
  elDynAttrNS' ns tag1 a1 a2
  elDynAttrNS' ns tag2 a3 a4
  elDynAttrNS' ns tag3 a5 a6
  elDynAttrNS' ns tag4 a7 a8
  elDynAttrNS' ns tag5 a9 a10
  elDynAttrNS' ns tag6 a11 a12
  elDynAttrNS' ns tag7 a13 a14
  elDynAttrNS' ns tag8 a15 a16
  return ()

el9Svg :: 
  (DomBuilder t m, PostBuild t m) => 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) ->
  m a -> 
  Text ->
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->  
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  m ()
el9Svg tag1 a1 a2 tag2 a3 a4 tag3 a5 a6 tag4 a7 a8 tag5 a9 a10 tag6 a11 a12 tag7 a13 a14 tag8 a15 a16 tag9 a17 a18 = do
  elDynAttrNS' ns tag1 a1 a2
  elDynAttrNS' ns tag2 a3 a4
  elDynAttrNS' ns tag3 a5 a6
  elDynAttrNS' ns tag4 a7 a8
  elDynAttrNS' ns tag5 a9 a10
  elDynAttrNS' ns tag6 a11 a12
  elDynAttrNS' ns tag7 a13 a14
  elDynAttrNS' ns tag8 a15 a16
  elDynAttrNS' ns tag9 a17 a18
  return ()

el10Svg :: 
  (DomBuilder t m, PostBuild t m) => 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) ->
  m a -> 
  Text ->
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->  
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a -> 
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  Text -> 
  Dynamic t (Map Text Text) -> 
  m a ->
  m ()
el10Svg tag1 a1 a2 tag2 a3 a4 tag3 a5 a6 tag4 a7 a8 tag5 a9 a10 tag6 a11 a12 tag7 a13 a14 tag8 a15 a16 tag9 a17 a18 tag10 a19 a20 = do
  elDynAttrNS' ns tag1 a1 a2
  elDynAttrNS' ns tag2 a3 a4
  elDynAttrNS' ns tag3 a5 a6
  elDynAttrNS' ns tag4 a7 a8
  elDynAttrNS' ns tag5 a9 a10
  elDynAttrNS' ns tag6 a11 a12
  elDynAttrNS' ns tag7 a13 a14
  elDynAttrNS' ns tag8 a15 a16
  elDynAttrNS' ns tag9 a17 a18
  elDynAttrNS' ns tag10 a19 a20
  return ()

ns :: Maybe Text
ns = Just "http://www.w3.org/2000/svg"