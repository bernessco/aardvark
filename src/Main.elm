module Main exposing (..)

import Browser
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map3, maybe, nullable, string)
import Process
import Task
import Time exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { board : Status (List Combined)
    , nextGame : Status NextGameResponse
    , endedGames : List EndedGame
    }


type alias EndedGame =
    { id : Int
    , result : Int
    }


type alias Combined =
    { id : String
    , color : String
    , position : Int
    }


type alias ConfigurationResponse =
    { results : List String
    , colors : List String
    , positionToId : List Int
    }


type alias NextGameResponse =
    { id : Int
    , fakeStartDelta : Int
    }


type alias GameResponse =
    { id : Int
    , result : Maybe Int
    }


type Status a
    = Failure
    | Loading
    | Success a


type Msg
    = CompletedConfigurationLoad (Result Http.Error ConfigurationResponse)
    | CompletedNextGameLoad (Result Http.Error NextGameResponse)
    | CompletedGameLoad (Result Http.Error GameResponse)
    | FetchGame Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Loading
      , nextGame = Loading
      , endedGames = []
      }
    , Cmd.batch
        [ fetchConfiguration
        , fetchNextGame
        ]
    )


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedConfigurationLoad (Ok config) ->
            ( { model | board = Success (combineResult config.results config.colors config.positionToId) }, Cmd.none )

        CompletedConfigurationLoad (Err _) ->
            ( { model | board = Failure }, Cmd.none )

        CompletedNextGameLoad (Ok nextGame) ->
            ( { model | nextGame = Success { id = nextGame.id, fakeStartDelta = nextGame.fakeStartDelta } }, Cmd.none )

        CompletedNextGameLoad (Err _) ->
            ( { model | nextGame = Failure }, Cmd.none )

        CompletedGameLoad (Ok game) ->
            case game.result of
                Nothing ->
                    ( model, delay 1000 (FetchGame game.id) )

                Just result ->
                    ( { model
                        | endedGames = { id = game.id, result = result } :: model.endedGames
                      }
                    , fetchNextGame
                    )

        CompletedGameLoad (Err _) ->
            ( { model | nextGame = Failure }, Cmd.none )

        FetchGame id ->
            ( model, fetchGame id )


findIndexById : String -> List Int -> Int
findIndexById id positionToId =
    let
        found =
            List.head <|
                List.filter (\x -> x >= 0) <|
                    List.indexedMap
                        (\a b ->
                            if String.fromInt b == id then
                                a

                            else
                                -1
                        )
                        positionToId
    in
    case found of
        Just position ->
            position

        Nothing ->
            -1


attachPositionToResult : List Combined -> List Int -> List Combined
attachPositionToResult combined positionToId =
    List.map
        (\record ->
            { record
                | position = findIndexById record.id positionToId
            }
        )
        combined


combineResult : List String -> List String -> List Int -> List Combined
combineResult results colors positionToId =
    List.map2 (\a b -> { id = a, color = b, position = -1 }) results colors
        |> (\list -> attachPositionToResult list positionToId)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.nextGame of
        Failure ->
            Sub.none

        Loading ->
            Sub.none

        Success nextGame ->
            Time.every (toFloat nextGame.fakeStartDelta * 1000) (\_ -> FetchGame nextGame.id)


view : Model -> Html Msg
view model =
    layout model


layout : Model -> Html Msg
layout model =
    div [ class "app-layout" ]
        [ div [ class "app-layout__sidebar" ]
            [ sidebar model ]
        , div [ class "app-layout__content" ]
            [ h1 [] [ text "Aardvark Roulette" ]
            , content model
            ]
        ]


renderList : List String -> Html Msg
renderList records =
    ul []
        (List.map (\record -> li [] [ text record ]) records)


sidebar : Model -> Html Msg
sidebar model =
    aside [ class "app-sidebar" ]
        [ h1 [] [ text "Sidebar" ]
        ]


content : Model -> Html Msg
content model =
    case model.board of
        Failure ->
            h3 [] [ text "Failed to load" ]

        Loading ->
            h3 [] [ text "Loading..." ]

        Success board ->
            div [ class "app-container" ]
                [ div [ class "app-container__captions" ]
                    [ div [ class "app-container__caption" ] []
                    , div [ class "app-container__caption" ]
                        [ text "Game board"
                        ]
                    , div [ class "app-container__caption" ] []
                    ]
                , div [ class "app-container__content" ]
                    [ div [ class "roulette-board" ]
                        [ ul [ class "roulette-board__row" ]
                            (List.map
                                (\record ->
                                    li
                                        [ classList
                                            [ ( "roulette-field", True )
                                            , ( "roulette-field--" ++ record.color, True )
                                            ]
                                        ]
                                        [ text record.id
                                        ]
                                )
                             <|
                                List.sortBy .position board
                            )
                        ]
                    ]
                ]


fetchConfiguration : Cmd Msg
fetchConfiguration =
    Http.get
        { url = "https://dev-games-backend.advbet.com/v1/ab-roulette/1/configuration"
        , expect = Http.expectJson CompletedConfigurationLoad configurationDecoder
        }


configurationDecoder : Decoder ConfigurationResponse
configurationDecoder =
    map3 ConfigurationResponse
        (field "results" (list string))
        (field "colors" (list string))
        (field "positionToId" (list int))


fetchNextGame : Cmd Msg
fetchNextGame =
    Http.get
        { url = "https://dev-games-backend.advbet.com/v1/ab-roulette/1/nextGame"
        , expect = Http.expectJson CompletedNextGameLoad nextGameDecoder
        }


nextGameDecoder : Decoder NextGameResponse
nextGameDecoder =
    map2 NextGameResponse
        (field "id" int)
        (field "fakeStartDelta" int)


fetchGame : Int -> Cmd Msg
fetchGame id =
    Http.get
        { url = "https://dev-games-backend.advbet.com/v1/ab-roulette/1/game/" ++ String.fromInt id
        , expect = Http.expectJson CompletedGameLoad gameDecoder
        }


gameDecoder : Decoder GameResponse
gameDecoder =
    map2 GameResponse
        (field "id" int)
        (field "result" (maybe int))
