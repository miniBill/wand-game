module Main exposing (main)

import Browser
import Element exposing (Attribute, Element, el, fill, height, px, scrollbarY, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Icons
import List.Extra


type alias Health =
    { north : Int
    , east : Int
    , south : Int
    , west : Int
    }


type alias Model =
    { players : PlayerNames
    , wands : WandNames
    , rounds : List Round
    }


type alias PlayerNames =
    { north : String
    , east : String
    , south : String
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


initHealth : Health
initHealth =
    { north = 3
    , east = 3
    , south = 3
    , west = 0
    }


emptyShot : Shot
emptyShot =
    { north = Nothing
    , east = Nothing
    , south = Nothing
    , west = Nothing
    }


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
    column
        [ width fill, height fill ]
        [ wrappedRow [ width fill ]
            [ viewPlayers model
            , viewWands model
            ]
        , viewRound model 0 model.rounds True initHealth []
            |> wrappedRow [ width fill, height fill, scrollbarY ]
        ]


viewRound :
    Model
    -> Int
    -> List Round
    -> Bool
    -> Health
    -> List (Element Msg)
    -> List (Element Msg)
viewRound model index rounds pastRoundComplete health acc =
    case rounds of
        [] ->
            if pastRoundComplete then
                viewRound model index [ emptyRound ] False health acc

            else
                List.reverse acc

        head :: tail ->
            let
                ( newHealth, elements, isComplete ) =
                    innerViewRound model index head health
            in
            viewRound model (index + 1) tail isComplete newHealth (elements :: acc)


innerViewRound :
    Model
    -> Int
    -> Round
    -> Health
    ->
        ( Health
        , Element Msg
        , Bool
        )
innerViewRound model index round health =
    ( health
    , column
        [ Border.width 1
        , padding
        ]
        [ el [ Font.bold ]
            (text ("Round " ++ String.fromInt (index + 1)))
        , viewInitialWands model round.initialWands
            |> Element.map
                (\newWands ->
                    { round | initialWands = newWands }
                )
        , viewShot model round 0 round.shots True health health []
            |> column [ width fill ]
        ]
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
    , List.length round.shots == 4
    )


viewInitialWands :
    Model
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
viewInitialWands model wands =
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
                            , text (wandName model.wands)
                            ]
                        )
                , onChange = \newPlayer -> setter newPlayer
                , options =
                    [ ( North, model.players.north )
                    , ( East, model.players.east )
                    , ( South, model.players.south )
                    , ( West, model.players.west )
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


viewShot :
    Model
    -> Round
    -> Int
    -> List Shot
    -> Bool
    -> Health
    -> Health
    -> List (Element Round)
    -> List (Element Round)
viewShot model round index shots pastShotComplete initialRoundHealth health acc =
    case shots of
        [] ->
            if pastShotComplete then
                viewShot model round index [ emptyShot ] False initialRoundHealth health acc

            else
                List.reverse acc

        head :: tail ->
            let
                ( newHealth, elements, isComplete ) =
                    innerViewShot model round index initialRoundHealth head health
            in
            viewShot model round (index + 1) tail isComplete initialRoundHealth newHealth (elements :: acc)


innerViewShot :
    Model
    -> Round
    -> Int
    -> Shot
    -> Health
    -> Health
    -> ( Health, Element Round, Bool )
innerViewShot model round index shot initialRoundHealth healths =
    let
        iif : Int -> a -> Maybe a
        iif c v =
            if c > 0 then
                Just v

            else
                Nothing

        viewShotPlayer :
            (PlayerNames -> String)
            -> (Health -> Int)
            -> (Shot -> Maybe Player)
            -> (Maybe Player -> msg)
            -> Maybe (Element msg)
        viewShotPlayer playerName health getter setter =
            iif (health initialRoundHealth) <|
                Input.radioRow
                    [ spacing
                    , width fill
                    ]
                    { label =
                        Input.labelLeft [ width (px 120) ]
                            (text (playerName model.players))
                    , onChange = \newPlayer -> setter newPlayer
                    , options =
                        [ iif initialRoundHealth.north ( Just North, model.players.north )
                        , iif initialRoundHealth.east ( Just East, model.players.east )
                        , iif initialRoundHealth.south ( Just South, model.players.south )
                        , iif initialRoundHealth.west ( Just West, model.players.west )
                        , iif 1 ( Nothing, "Air" )
                        ]
                            |> List.filterMap identity
                            |> List.map (\( value, label ) -> Input.option value (text label))
                    , selected = Just (getter shot)
                    }
    in
    ( healths
    , column
        [ Border.width 1
        , padding
        ]
        [ text ("Shot " ++ String.fromInt (index + 1))
        , [ viewShotPlayer .north .north .north <| \newPlayer -> { shot | north = newPlayer }
          , viewShotPlayer .east .east .east <| \newPlayer -> { shot | east = newPlayer }
          , viewShotPlayer .south .south .south <| \newPlayer -> { shot | south = newPlayer }
          , viewShotPlayer .west .west .west <| \newPlayer -> { shot | west = newPlayer }
          ]
            |> List.filterMap identity
            |> column []
        ]
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
    , index < 3
    )


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
