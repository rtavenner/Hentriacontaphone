port module Main exposing (..)

import Html exposing (program)
import Html.Attributes as Html
import Svg exposing (svg,text,text_)
import Svg.Attributes as S
import Dict exposing (Dict)
import Set exposing (Set)
import Task
import Window

type alias Model = {keys : Set (Int,Int), window : Maybe Window.Size}

port keydown : (Int,Int) -> Cmd msg
port keyup   : (Int,Int) -> Cmd msg

port downs : (Int -> msg) -> Sub msg
port ups   : (Int -> msg) -> Sub msg

type Msg
  = NoOp
  | WindowResize (Window.Size)
  | Down (Int,Int)
  | Up   (Int,Int)

main : Program Never Model Msg
main = 
  let list = List.concatMap (\x -> List.map (\y -> (x,y)) (List.range -3 6)) (List.range -6 10) -- [(x,y) | x <- [-6..10], y <- [-3..6]]
  in Html.program
    { init = ({keys = Set.empty, window = Nothing}, Task.perform WindowResize Window.size)
    , view = \{keys,window} ->
      case window of
        Nothing -> svg [] []
        Just {width,height} ->
          let
            keyWidth = toFloat width / 28
            keyHeight = toFloat height / 4
          in
            svg 
              [ Html.width (width - 20)
              , Html.height (height - 26)
              , Html.style [("border","3px solid black")]]
              (List.map 
                (\(x,y) -> 
                  Svg.rect 
                    [ S.width (toString keyWidth)
                    , S.height (toString keyHeight)
                    , S.x (toString (keyWidth * (7 + 2*toFloat x + toFloat y)))
                    , S.y (toString (keyHeight/2 * (4.5 - toFloat y)))
                    , S.fill -- <| keyColor keys x y
                      (if Set.member (x,y) keys
                        then "red"
                        else keyColor (5*x+3*y))
                    , S.stroke "black"
                    ] 
                    [])
                list)
  , update = \msg mod -> 
    case msg of
      NoOp -> (mod, Cmd.none)
      Down idx -> ({mod | keys = Set.insert idx mod.keys}, keydown idx)
      Up   idx -> ({mod | keys = Set.remove idx mod.keys}, keyup   idx)
      WindowResize w -> ({mod | window = Just w}, Cmd.none)
  , subscriptions = \_ ->
    Sub.batch
      [ downs (flip Dict.get keyIndices >> Maybe.map Down >> Maybe.withDefault NoOp)
      , ups   (flip Dict.get keyIndices >> Maybe.map Up   >> Maybe.withDefault NoOp)
      , Window.resizes WindowResize
      ]
  }

keyColor : Int -> String
keyColor n = 
  let nn = n % 31
  in if List.member nn [0,5,8,13,18,23,26]
  then "#FFFFFF"
  else if List.member nn [3,11,16,21,29, 2,10,15,20,28]
  then "#404040"
  else "#8080FF"


keyIndices : Dict Int (Int,Int)
keyIndices =
  Dict.fromList
    [ (27 ,(-5,4))
    , (112,(-4,4)) --F keys set up to work with chromebook keyboards
    , (113,(-3,4))
    , (114,(-2,4))
    , (115,(-1,4))
    , (116,( 1,4))
    , (117,( 2,4))
    , (118,( 3,4))
    , (119,( 4,4))
    , (120,( 6,4))
    , (121,( 7,4))

    , (192,(-5,3))
    , (49,(-4,3))
    , (50,(-3,3))
    , (51,(-2,3))
    , (52,(-1,3))
    , (53,( 0,3))
    , (54,( 1,3))
    , (55,( 2,3))
    , (56,( 3,3))
    , (57,( 4,3))
    , (48,( 5,3))
    , (173,(6,3))   -- different on different browsers
    , (189,(6,3))   -- different on different browsers
    , (61,( 7,3))   -- different on different browsers
    , (187,(7,3))   -- different on different browsers
    , ( 8,( 8,3))

    , ( 9,(-4,2))
    , (81,(-3,2))
    , (87,(-2,2))
    , (69,(-1,2))
    , (82,( 0,2))
    , (84,( 1,2))
    , (89,( 2,2))
    , (85,( 3,2))
    , (73,( 4,2))
    , (79,( 5,2))
    , (80,( 6,2))
    , (219,(7,2))
    , (221,(8,2))
    , (220,(9,2))

    , (65,(-2,1))
    , (83,(-1,1))
    , (68,( 0,1))
    , (70,( 1,1))
    , (71,( 2,1))
    , (72,( 3,1))
    , (74,( 4,1))
    , (75,( 5,1))
    , (76,( 6,1))
    , (59,( 7,1))   -- different on different browsers
    , (186,(7,1))   -- different on different browsers
    , (222,(8,1))   

    , (90,(-1,0))
    , (88,( 0,0))
    , (67,( 1,0))
    , (86,( 2,0))
    , (66,( 3,0))
    , (78,( 4,0))
    , (77,( 5,0))
    , (188,(6,0))
    , (190,(7,0))
    , (191,(8,0))
    ]