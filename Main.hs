{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
module Main where

import           Data.Aeson
import           GHC.Generics
import           Data.Bool
import qualified Data.Map as M

import           Miso
import qualified Miso.Svg as SVG
import           Miso.String  (MisoString)
import qualified Miso.String  as S
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Language.Javascript.JSaddle.Warp as JSaddle

main :: IO ()
main = JSaddle.run 8080 $ startApp App { initialAction = Id, ..}
  where
    model = LoginScreen ""
    events = defaultEvents
    subs = [ websocketSub uri protocols HandleWebSocket ]
    update = updateModel
    view = appView
    uri = URL "ws://127.0.0.1:9160/"
    protocols = Protocols [ ]
    mountPoint = Nothing

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleWebSocket (WebSocketMessage (Message m))) model
  = case msg of _ | "other " `T.isPrefixOf` msg -> model <# do pure (AOtherPlayer (S.ms . T.drop 6 $ msg) )
                  | "ready" == msg -> model <# do pure AReady
                  | "Rock" == msg -> model <# do pure $ AOtherChoseMove Rock
                  | "Paper" == msg -> model <# do pure $ AOtherChoseMove Paper
                  | "Scissors" == msg -> model <# do pure $ AOtherChoseMove Scissors
                  | otherwise -> noEff model
    where msg :: Text
          msg = S.fromMisoString m
--updateModel (SendMessage msg) model = model <# do send msg >> pure Id

updateModel (AUpdateName m) (LoginScreen _) = noEff $ LoginScreen { name = m }
updateModel ASendName       (LoginScreen m) = PairScreen { name = m , pairing = False } <# do send (Message m) >> pure Id 
updateModel APair           ps = ps { pairing = True} <# do send (Message $ S.ms "pair") >> pure Id 
updateModel (AOtherPlayer nm) PairScreen{..} = 
                           noEff $ SelectMoveScreen { myName = name 
                                                    , opponentName = nm 
                                                    , myScore = 0
                                                    , opponentScore = 0}
updateModel (AChoseMove mv) SelectMoveScreen{..} = 
                              MoveChosenScreen { myName = myName
                                               , opponentName = opponentName 
                                               , myScore = myScore
                                               , opponentScore = opponentScore 
                                               , myMove = mv } <# do send (Message $ mvToText mv) >> pure Id

updateModel (AOtherChoseMove mv)  MoveChosenScreen{..} = noEff $ newState
  where resRound = RoundResult { myName = myName
                               , opponentName = opponentName 
                               , myScore = myScore
                               , opponentScore = opponentScore 
                               , myMove = myMove
                               , opponentMove = mv }
        newState = case moveToResult myMove mv of Win -> resRound { myScore = myScore + 1}
                                                  Draw -> resRound
                                                  Lose -> resRound { opponentScore = opponentScore +1}



updateModel AReady RoundResult{..} = 
  noEff $ mbEnd myScore opponentScore


  where mbEnd 2 _ = GameResult { myName = myName 
                               , opponentName = opponentName
                               , myScore = myScore
                               , opponentScore = opponentScore
                               , result = Win }
        mbEnd _ 2 = GameResult { myName = myName 
                               , opponentName = opponentName
                               , myScore = myScore
                               , opponentScore = opponentScore
                               , result = Lose }
        mbEnd _ _ = SelectMoveScreen { myName = myName 
                                     , opponentName = opponentName 
                                     , myScore = myScore
                                     , opponentScore = opponentScore }

updateModel AReady GameResult{..} = 
  noEff $ PairScreen{ name = myName 
                    , pairing = False }

updateModel _ model = noEff model

instance ToJSON Message
instance FromJSON Message

newtype Message = Message MisoString
  deriving (Eq, Show, Generic)

data Action
  = HandleWebSocket (WebSocket Message)
  | AReady
  | AUpdateName MisoString
  | ASendName
  | AOtherPlayer MisoString
  | APair
  | AChoseMove Move
  | AOtherChoseMove Move
  | Id

data Move = Rock | Paper | Scissors deriving (Show , Read , Eq)
data RResult = Win | Draw | Lose deriving (Eq , Show)



data Model = LoginScreen { name :: MisoString } 
           | PairScreen { name :: MisoString 
                        , pairing :: Bool }
           | SelectMoveScreen { myName :: MisoString 
                              , opponentName :: MisoString 
                              , myScore :: Int 
                              , opponentScore :: Int }
           | MoveChosenScreen { myName :: MisoString 
                              , opponentName :: MisoString 
                              , myScore :: Int 
                              , opponentScore :: Int
                              , myMove :: Move } 
           | RoundResult { myName :: MisoString 
                         , opponentName :: MisoString 
                         , myScore :: Int 
                         , opponentScore :: Int
                         , myMove :: Move
                         , opponentMove :: Move } 
           | GameResult { myName :: MisoString 
                        , opponentName :: MisoString 
                        , myScore :: Int 
                        , opponentScore :: Int 
                        , result :: RResult } deriving (Show , Eq)
           
appView :: Model -> View Action
appView (LoginScreen m) = div_ [ class_ "container" , style_ $ M.fromList [("text-align", "center")]]
                               [ div_ [ style_ $ M.fromList [("font-size", "2rem"),
                                                             ("font-weight" ,"100"),
                                                             ("color", "#97cbff"),
                                                             ("padding-top","300px")]]
                                      [ input_ [type_ "text" , onInput AUpdateName , onEnter ASendName , class_ "input-style" , placeholder_ "name"]] 
                               , div_ [ class_ "button-blue" , onClick ASendName]
                                      [ text "Join"]
                               , link_ [ rel_ "stylesheet", href_ "style.css"]
                               , link_ [ rel_ "stylesheet", href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"]

                               ]

appView PairScreen{..} = div_ [ class_ "container" , style_ $ M.fromList [("text-align", "center")]]
                              [ div_ [ style_ $ M.fromList [("font-size", "2.7rem"),
                                                            ("font-weight", "100"),
                                                            ("color", "#97cbff"), 
                                                            ("padding-top","300px")]]

                                     [ text $ name]
                              , div_ [ style_ $ M.fromList [("font-size", "1rem"),
                                                            ("font-weight", "100"),
                                                            ("color", "#97cbff") ]]

                                     [ text $ "37 players online"]
                              , viewPairing pairing
                              , link_ [ rel_ "stylesheet", href_ "style.css"]
                              , link_ [ rel_ "stylesheet", href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"]
                              ]
                         where viewPairing False = div_ [ class_ "button-blue" , onClick APair] [ text $ "Play Game" ]
                               viewPairing True = div_ [ class_ "loader"] []

appView SelectMoveScreen{..} =
    div_ [ class_ "container-fluid"]
         [ div_ [ class_ "row"]
                [ div_ [ class_ "col-2"]
                       [ div_ [ style_ $ M.fromList [("height","405px"),("width","100%")]] []
                       , div_ [ style_ $ M.fromList [("height","150px"),("width","100%")]] 
                              [ div_ [ style_ $ M.fromList [("height","75px"),("width","300px"),("border-bottom","2px solid #c9e4ff"),("display","table-cell"),("vertical-align", "bottom"),("font-size","2.7rem"),("font-weight", "100"),("color", "#97cbff")]] 
                                     [ text $ myName
                                     , div_ [ class_ "score-style"] [text $ S.ms . show $ myScore] 
                                     ]
                              , div_ [ class_ "name-style"]
                                     [ text $ opponentName
                                     , div_ [ class_ "score-style"] [text $ S.ms . show $ opponentScore]]
                              ]
                       ]
                , div_ [ class_ "col-10"] 
                       [ div_ [ class_ "container-fluid" , style_ $ M.fromList [("padding","0px 60px")]]
                              [ div_ [ class_ "row justify-content-md-center" , style_ $ M.fromList [("margin-top","150px")]]
                                     [ div_ [ class_ "col-12-sm"]
                                            [ div_ [ class_ "cont"]
                                                   [  ]
                                            ]
                                     ]
                              , div_ [ style_ $ M.fromList [("height","150px"),("width","100%")]]
                                     [ div_ [ style_ $ M.fromList [("height","75px"),("width","100%"),("border-bottom","2px solid #c9e4ff"),("text-align","center")]] []
                                     ]
                              , div_ [ class_ "row justify-content-md-center" ]
                                     [ div_ [ class_ "col-12-sm"]
                                            [ div_ [ class_ "cont" , onClick (AChoseMove Paper)]
                                                   [ paper "st0"]
                                            , div_ [ class_ "cont" , onClick (AChoseMove Scissors)]
                                                   [ scissors "st0"]
                                            , div_ [ class_ "cont" , onClick (AChoseMove Rock)]
                                                   [ rock "st0"]
                                            ]
                                     ]

                              ]
                       ]
                ]
         , link_ [ rel_ "stylesheet", href_ "style.css"]
         , link_ [ rel_ "stylesheet", href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" ]
         ]
---                              , button_ [ onClick (AChoseMove Rock)] [ text (S.pack "Rock")]



appView MoveChosenScreen{..} = 
    div_ [ class_ "container-fluid"]
         [ div_ [ class_ "row"]
                [ div_ [ class_ "col-2"]
                       [ div_ [ style_ $ M.fromList [("height","405px"),("width","100%")]] []
                       , div_ [ style_ $ M.fromList [("height","150px"),("width","100%")]] 
                              [ div_ [ style_ $ M.fromList [("height","75px"),("width","300px"),("border-bottom","2px solid #c9e4ff"),("display","table-cell"),("vertical-align", "bottom"),("font-size","2.7rem"),("font-weight", "100"),("color", "#97cbff")]] 
                                     [ text $ myName
                                     , div_ [ class_ "score-style"] [text $ S.ms . show $ myScore] 
                                     ]
                              , div_ [ class_ "name-style"]
                                     [ text $ opponentName
                                     , div_ [ class_ "score-style"] [text $ S.ms . show $ opponentScore]]
                              ]
                       ]
                , div_ [ class_ "col-10"] 
                       [ div_ [ class_ "container-fluid" , style_ $ M.fromList [("padding","0px 60px")]]
                              [ div_ [ class_ "row justify-content-md-center" , style_ $ M.fromList [("margin-top","150px")]]
                                     [ div_ [ class_ "col-12-sm"]
                                            [ div_ [ class_ "cont"]
                                                   [ ]
                                            ]
                                     ]
                              , div_ [ style_ $ M.fromList [("height","150px"),("width","100%")]]
                                     [ div_ [ style_ $ M.fromList [("height","75px"),("width","100%"),("border-bottom","2px solid #c9e4ff"),("text-align","center")]] []
                                     ]
                              , div_ [ class_ "row justify-content-md-center" ]
                                     [ div_ [ class_ "col-12-sm"]
                                            [ div_ [ class_ "cont" ]
                                                   [ paper (moveToClass myMove Paper)]
                                            , div_ [ class_ "cont" ]
                                                   [ scissors (moveToClass myMove Scissors)]
                                            , div_ [ class_ "cont" ]
                                                   [ rock (moveToClass myMove Rock)]
                                            ]
                                     ]

                              ]
                       ]
                ]
         , link_ [ rel_ "stylesheet", href_ "style.css"]
         , link_ [ rel_ "stylesheet", href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" ]
         ]

appView RoundResult{..} = 
    div_ [ class_ "container-fluid"]
         [ div_ [ class_ "row"]
                [ div_ [ class_ "col-2"]
                       [ div_ [ style_ $ M.fromList [("height","405px"),("width","100%")]] []
                       , div_ [ style_ $ M.fromList [("height","150px"),("width","100%")]] 
                              [ div_ [ style_ $ M.fromList [("height","75px"),("width","300px"),("border-bottom","2px solid #c9e4ff"),("display","table-cell"),("vertical-align", "bottom"),("font-size","2.7rem"),("font-weight", "100"),("color", "#97cbff")]] 
                                     [ text $ myName
                                     , div_ [ class_ "score-style"] [text $ S.ms . show $ myScore] 
                                     ]
                              , div_ [ class_ "name-style"]
                                     [ text $ opponentName
                                     , div_ [ class_ "score-style"] [text $ S.ms . show $ opponentScore]]
                              ]
                       ]
                , div_ [ class_ "col-10"] 
                       [ div_ [ class_ "container-fluid" , style_ $ M.fromList [("padding","0px 60px")]]
                              [ div_ [ class_ "row justify-content-md-center" , style_ $ M.fromList [("margin-top","150px")]]
                                     [ div_ [ class_ "col-12-sm"]
                                            [ div_ [ class_ "cont"]
                                                   [ moveToSVG opponentMove ]
                                            ]
                                     ]
                              , div_ [ style_ $ M.fromList [("height","150px"),("width","100%")]]
                                     [ div_ [ style_ $ M.fromList [("height","75px"),("width","100%"),("border-bottom","2px solid #c9e4ff"),("text-align","center")]] []
                                     ]
                              , div_ [ class_ "row justify-content-md-center" ]
                                     [ div_ [ class_ "col-12-sm"]
                                            [ div_ [ class_ "cont" ]
                                                   [ paper (moveToClass myMove Paper)]
                                            , div_ [ class_ "cont" ]
                                                   [ scissors (moveToClass myMove Scissors)]
                                            , div_ [ class_ "cont" ]
                                                   [ rock (moveToClass myMove Rock)]
                                            ]
                                     ]

                              ]
                       ]
                ]
         , link_ [ rel_ "stylesheet", href_ "style.css"]
         , link_ [ rel_ "stylesheet", href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" ]
         ]

appView GameResult{..} = 
                         div_ [ class_ "container" , style_ $ M.fromList [("text-align", "center")]]
                              [ div_ [ style_ $ M.fromList [("font-size", "2.7rem"),
                                                            ("font-weight", "100"),
                                                            ("color", "#97cbff"), 
                                                            ("padding-top","300px")]]

                                     [ div_ [ style_ $ M.fromList [("display", "inline-block"),("margin-right", "200px")]]
                                            [ text $ myName
                                            , br_ []
                                            , text $ S.ms . show $ myScore 
                                            ]
                                     , div_ [ style_ $ M.fromList [("display", "inline-block")]]
                                            [ text $ opponentName
                                            , br_ []
                                            , text $ S.ms . show $ opponentScore 
                                            ]
                                     ] 
                                     
                              , div_ [ style_ $ M.fromList [("font-size", "2rem"),
                                                            ("font-weight", "100"),
                                                            ("color", "#97cbff") ]]

                                     [ text $ S.ms $ "You " ++ (show result)]
                              , link_ [ rel_ "stylesheet", href_ "style.css"]
                              , link_ [ rel_ "stylesheet", href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"]
                              ]


appView _ = div_ [ style_ $ M.fromList [("text-align", "center")] ] 
                 [ h1_ [style_ $ M.fromList [("font-weight", "bold")] ] 
                       [ text (S.pack "Intr-o stare") ] ]





onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool Id action . (== KeyCode 13)

mvToText :: Move -> MisoString
mvToText Rock = S.ms "rock"
mvToText Paper = S.ms "paper"
mvToText Scissors = S.ms "scissors" 

moveToResult :: Move -> Move -> RResult
moveToResult Scissors Paper = Win
moveToResult Paper Rock = Win
moveToResult Rock Scissors = Win
moveToResult Rock Rock = Draw
moveToResult Paper Paper = Draw
moveToResult Scissors Scissors = Draw
moveToResult _ _ = Lose

moveToSVG :: Move -> View Action
moveToSVG Rock = rock "st0"
moveToSVG Paper = paper "st0"
moveToSVG Scissors = scissors "st0"

paper cls = 
        SVG.svg_ [ SVG.id_ "Layer_1" , SVG.viewBox_ "0 0 150 150"]
                 [ SVG.path_ [ SVG.class_' cls 
                             , SVG.d_ "M65.1,77.8V28.1c0-1.9-0.8-3.7-2.1-4.9c-1.3-1.3-3-2.1-4.9-2.1c-3.8,0-7,3.2-7,7v41c0,1.9,0.8,34.8,0,30.3  c0,0-7.2-9.3-5.7-13.1s6.1-21.7-0.2-23.9c-6.3-2.1-9.8,1.6-9.8,1.6S29.6,82,27.9,84.4c-1.1,1.7,1.5,14.5,3.3,19.5s12.5,33.7,24,37.7  s14.2,2.8,14.2,2.8l30.5,0.2c0,0,14.8-3.7,15.3-16.9s-0.3-88.3,0-91.3s-9.3-11.8-16.3,0" ] []
                 , SVG.path_ [ SVG.class_' cls 
                             ,SVG.d_ "M65.1,69.1" ] []
                 , SVG.path_ [ class_ cls 
                             , SVG.d_ "M67.1,77.8V13.1c0-3.5,2.9-6.3,6.5-6.3h1.6c3.6,0,6.5,2.8,6.5,6.3v70.4" ] []
                 , SVG.path_ [ SVG.class_' cls 
                             , SVG.d_ "M84,83.5V15.6c0-3.4,3-6.1,6.7-6.1h1.7c3.7,0,6.7,2.7,6.7,6.1v67.9" ] []
                 , SVG.line_ [ SVG.class_' cls , SVG.x1_ "100.5" , SVG.x2_ "100.5" , SVG.y1_ "83.5" , SVG.y2_ "34.2"] []] 

scissors cls = 
        SVG.svg_ [ SVG.id_ "Layer_1" , SVG.viewBox_ "0 0 150 150"]
                 [ SVG.path_ [ SVG.class_' cls 
                             , SVG.d_ "M90.2,102.8l-1.8-0.1c-3.5-0.2-6.2-3.2-5.9-6.7l2-32.9c0.2-3.5,3.2-6.2,6.7-5.9l1.8,0.1  c3.5,0.2,6.2,3.2,5.9,6.7l-2,32.9C96.7,100.3,93.7,103,90.2,102.8z" ] []
                 , SVG.path_ [ SVG.class_' cls 
                             ,SVG.d_ "M106.3,97l-0.3,0c-3.5-0.2-6.2-3.2-5.9-6.7l1.2-19.7c0.2-3.5,3.2-6.2,6.7-5.9l0.3,0c3.5,0.2,6.2,3.2,5.9,6.7  L113,91.1C112.7,94.6,109.7,97.3,106.3,97z" ] []
                 , SVG.path_ [ class_ cls 
                             , SVG.d_ "M66.8,72.7l11-63.5c0.6-3.4,3.9-5.7,7.3-5.1l0,0c3.4,0.6,5.7,3.9,5.1,7.3l-8.7,59.8" ] []
                 , SVG.path_ [ SVG.class_' cls 
                             , SVG.d_ "M50.3,77L36.8,13.7c-0.8-3.4,1.4-6.8,4.8-7.6l0,0c3.4-0.8,6.8,1.4,7.6,4.8l17.5,61.7" ] []
                 , SVG.path_ [ SVG.class_' cls 
                             , SVG.d_ "M113,91.1c0,10.1-0.2,27,0,31.6c0.5,16-8.4,24.7-15,24.4c-12.3-0.4-17-0.5-17.8-0.4c-0.8,0-8.5,1-14.8,0  c-6.3-1-21.3-17-21.3-20.2s-6.8-24.5-5.5-29S45.5,80.3,49,78.7c3.5-1.7,24.3-9.2,25.3-10.2s6.5-2.3,7.3,2.8s0.3,10.2-5.5,13.1  s-13.5,7.2-14.3,8.2s-3.5,6.1-1,10.2s10.5,12.6,10.5,12.6" ] []
                 ] 
rock cls = 
        SVG.svg_ [ SVG.id_ "Layer_1" , SVG.viewBox_ "0 0 150 150"]
                 [ SVG.path_ [ SVG.class_' cls 
                             , SVG.d_ "M64.2,74.5h-0.3c-3.5,0-6.3-2.8-6.3-6.3V39.1c0-3.5,2.8-6.3,6.3-6.3h0.3c3.5,0,6.3,2.8,6.3,6.3v29.1  C70.5,71.7,67.7,74.5,64.2,74.5z" ] []
                 , SVG.path_ [ SVG.class_' cls 
                             ,SVG.d_ "M79.5,78h-0.3c-3.5,0-6.3-2.8-6.3-6.3v-40c0-3.5,2.8-6.3,6.3-6.3h0.3c3.5,0,6.3,2.8,6.3,6.3v40  C85.8,75.2,83,78,79.5,78z" ] []
                 , SVG.path_ [ class_ cls 
                             , SVG.d_ "M95.6,78.4h-0.5c-3.5,0-6.3-2.8-6.3-6.3V33.3c0-3.5,2.8-6.3,6.3-6.3h0.5c3.5,0,6.3,2.8,6.3,6.3V72  C101.9,75.5,99.1,78.4,95.6,78.4z" ] []
                 , SVG.path_ [ SVG.class_' cls 
                             , SVG.d_ "M111.6,73.8h-0.4c-3.5,0-6.3-2.8-6.3-6.3V39.8c0-3.5,2.8-6.3,6.3-6.3h0.4c3.5,0,6.3,2.8,6.3,6.3v27.7  C117.9,71,115,73.8,111.6,73.8z" ] []
                 , SVG.path_ [ SVG.class_' cls 
                             , SVG.d_ "M117.7,68.7c0,0,0,24.3,0,29.3s0,17-4.7,20.7c-4.7,3.7-12.7,7.7-20.3,7C85,125,70.3,126.8,64,124  s-21-17.3-23.3-26.8C38.3,87.7,31,72.7,31.3,71c0.3-1.7,5.3-11,7-15c1.7-4,2-11.7,2.3-13c0.3-1.3,6.3-2.7,8.3,2.3s4,11.3,3.7,18  c-0.3,6.7-3,12-1,15.7s20.7,3,27.7,14.3" ] []
                 , SVG.line_ [ SVG.class_' cls , SVG.x1_ "57.7" , SVG.x2_ "57.7" , SVG.y1_ "81.7" , SVG.y2_ "57.7"] []
                 ] 
moveToClass mv1 mv2 = if mv1 == mv2 then "st0"
                                    else "st1"