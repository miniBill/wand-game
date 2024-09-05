module Main exposing (main)

import Browser
import Element exposing (Attribute, Element, el, fill, height, px, rgb, scrollbarY, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Icons
import List.Extra



-- wand order:


type alias Model =
    { players : PlayerNames
    , wands : WandNames
    , rounds : List Round
    }


type alias PlayerNames =
    { north : String
    , south : String
    , east : String
    , west : String
    }


type alias WandNames =
    { kill : String
    , heal : String
    , shield : String
    , neutral : String
    }


type alias Round =
    { initialWands :
        { kill : Player
        , heal : Player
        , shield : Player
        , neutral : Player
        }
    , shots : List Shot
    }


type alias Shot =
    { north : Maybe Player
    , east : Maybe Player
    , south : Maybe Player
    , west : Maybe Player
    }


type alias Msg =
    Model


type Player
    = North
    | East
    | South
    | West


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view =
            Element.layout
                [ padding
                , height fill
                ]
                << view
        , update = always
        }


init : Model
init =
    { players = initPlayers
    , wands = initWands
    , rounds = []
    }


initWands : WandNames
initWands =
    { kill = "Biscoff"
    , neutral = "Sheep"
    , shield = "Remote"
    , heal = "Whisk"
    }


initPlayers : PlayerNames
initPlayers =
    { north = "Alex"
    , east = "Brinley"
    , south = "Leonardo"
    , west = "Matt"
    }


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column (spacing :: attrs)


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row (spacing :: attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Element.wrappedRow (spacing :: attrs)


spacing : Attribute msg
spacing =
    Element.spacing 8


padding : Attribute msg
padding =
    Element.padding 8


view : Model -> Element Msg
view model =
    column [ width fill, height fill ]
        [ wrappedRow [ width fill ]
            [ viewPlayers model
            , viewWands model
            ]
        , (model.rounds ++ [ emptyRound ])
            |> List.indexedMap
                (\index round ->
                    column
                        (if index == List.length model.rounds then
                            [ Border.width 1
                            , padding
                            , Background.color (rgb 0.8 0.8 0.8)
                            ]

                         else
                            [ Border.width 1
                            , padding
                            ]
                        )
                        [ el [ Font.bold ]
                            (text ("Round " ++ String.fromInt (index + 1)))
                        , viewRound model.players model.wands round
                            |> Element.map
                                (\newRound ->
                                    { model
                                        | rounds =
                                            if index == List.length model.rounds then
                                                model.rounds ++ [ newRound ]

                                            else
                                                List.Extra.setAt index newRound model.rounds
                                    }
                                )
                        ]
                )
            |> column [ width fill, height fill, scrollbarY ]
        ]


viewRound : PlayerNames -> WandNames -> Round -> Element Round
viewRound playerNames wandNames round =
    column []
        [ viewInitialWands playerNames wandNames round.initialWands
            |> Element.map
                (\newWands ->
                    { round | initialWands = newWands }
                )
        , (round.shots ++ [ emptyShot ])
            |> List.indexedMap
                (\index shot ->
                    column
                        (if index == List.length round.shots then
                            [ Border.width 1
                            , padding
                            , Background.color (rgb 0.8 0.8 0.8)
                            ]

                         else
                            [ Border.width 1
                            , padding
                            ]
                        )
                        [ text ("Shot " ++ String.fromInt (index + 1))
                        , viewShot playerNames shot
                            |> Element.map
                                (\newShot ->
                                    { round
                                        | shots =
                                            if index == List.length round.shots then
                                                round.shots ++ [ newShot ]

                                            else
                                                List.Extra.setAt index newShot round.shots
                                    }
                                )
                        ]
                )
            |> column [ width fill ]
        ]


emptyShot : Shot
emptyShot =
    { north = Nothing
    , east = Nothing
    , south = Nothing
    , west = Nothing
    }


viewInitialWands :
    PlayerNames
    -> WandNames
    ->
        { kill : Player
        , heal : Player
        , shield : Player
        , neutral : Player
        }
    ->
        Element
            { kill : Player
            , heal : Player
            , shield : Player
            , neutral : Player
            }
viewInitialWands playerNames wandNames wands =
    let
        viewInitialWand :
            Element msg
            -> (WandNames -> String)
            -> ({ kill : Player, heal : Player, shield : Player, neutral : Player } -> Player)
            -> (Player -> msg)
            -> Element msg
        viewInitialWand wandKind wandName getter setter =
            Input.radioRow [ spacing, width fill ]
                { label =
                    Input.labelLeft [ width (px 120) ]
                        (row []
                            [ wandKind
                            , text
                                (wandName wandNames)
                            ]
                        )
                , onChange = \newPlayer -> setter newPlayer
                , options =
                    [ ( North, playerNames.north )
                    , ( East, playerNames.east )
                    , ( South, playerNames.south )
                    , ( West, playerNames.west )
                    ]
                        |> List.map (\( value, label ) -> Input.option value (text label))
                , selected = Just (getter wands)
                }
    in
    column []
        [ text "Initial wands"
        , viewInitialWand Icons.kill .kill .kill <| \newPlayer -> { wands | kill = newPlayer }
        , viewInitialWand Icons.heal .heal .heal <| \newPlayer -> { wands | heal = newPlayer }
        , viewInitialWand Icons.shield .shield .shield <| \newPlayer -> { wands | shield = newPlayer }
        , viewInitialWand Icons.neutral .neutral .neutral <| \newPlayer -> { wands | neutral = newPlayer }
        ]


viewShot : PlayerNames -> Shot -> Element Shot
viewShot playerNames shot =
    let
        viewShotPlayer playerName getter setter =
            Input.radioRow [ spacing, width fill ]
                { label =
                    Input.labelLeft [ width (px 120) ]
                        (text (playerName playerNames))
                , onChange = \newPlayer -> setter newPlayer
                , options =
                    [ ( Just North, playerNames.north )
                    , ( Just East, playerNames.east )
                    , ( Just South, playerNames.south )
                    , ( Just West, playerNames.west )
                    , ( Nothing, "Air" )
                    ]
                        |> List.map (\( value, label ) -> Input.option value (text label))
                , selected = Just (getter shot)
                }
    in
    column []
        [ viewShotPlayer .north .north <| \newPlayer -> { shot | north = newPlayer }
        , viewShotPlayer .east .east <| \newPlayer -> { shot | east = newPlayer }
        , viewShotPlayer .south .south <| \newPlayer -> { shot | south = newPlayer }
        , viewShotPlayer .west .west <| \newPlayer -> { shot | west = newPlayer }
        ]


emptyRound : Round
emptyRound =
    { initialWands =
        { kill = North
        , heal = East
        , shield = South
        , neutral = West
        }
    , shots = []
    }


viewPlayers : Model -> Element Msg
viewPlayers ({ players } as model) =
    let
        viewPlayer :
            (PlayerNames -> String)
            -> (String -> PlayerNames)
            -> Element Model
        viewPlayer getter setter =
            Input.text [ width fill ]
                { placeholder = Nothing
                , onChange = \newName -> { model | players = setter newName }
                , text = getter model.players
                , label = Input.labelHidden "Player name"
                }
    in
    column [ width fill ]
        [ el [ Font.bold ] (text "Player names")
        , viewPlayer .north <| \newName -> { players | north = newName }
        , viewPlayer .east <| \newName -> { players | east = newName }
        , viewPlayer .south <| \newName -> { players | south = newName }
        , viewPlayer .west <| \newName -> { players | west = newName }
        ]


viewWands : Model -> Element Msg
viewWands ({ wands } as model) =
    let
        viewWand :
            Element Model
            -> (WandNames -> String)
            -> (String -> WandNames)
            -> Element Model
        viewWand label getter setter =
            Input.text [ width fill ]
                { placeholder = Nothing
                , onChange = \newName -> { model | wands = setter newName }
                , text = getter model.wands
                , label = Input.labelLeft [] label
                }
    in
    column [ width fill ]
        [ el [ Font.bold ] (text "Wand names")
        , viewWand Icons.kill .kill <| \newName -> { wands | kill = newName }
        , viewWand Icons.heal .heal <| \newName -> { wands | heal = newName }
        , viewWand Icons.shield .shield <| \newName -> { wands | shield = newName }
        , viewWand Icons.neutral .neutral <| \newName -> { wands | neutral = newName }
        ]
