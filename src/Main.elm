module Main exposing (main, view)

import Browser
import Html exposing (Html, button, div, form, input, label, li, text, ul)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onBlur, onCheck, onInput, onSubmit)
import Time


type alias Task =
    { task : String
    , completedAt : Maybe Time.Posix
    }


type alias Model =
    { time : Time.Posix
    , tasks : List Task
    , task : String
    , error : Maybe String
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { task = "", tasks = [], error = Nothing, time = Time.millisToPosix 0 }, Cmd.none )


type Msg
    = SubmitForm
    | Tick Time.Posix
    | InputTask String
    | InputCompleteToggle Task Bool
    | ValidateTask


createTask : String -> Task
createTask task =
    { task = task, completedAt = Nothing }


validateTask : String -> Maybe String
validateTask task =
    if String.length (String.trim task) < 3 then
        Just "Not enough input"

    else
        Nothing


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        InputTask task ->
            ( { model | task = task, error = validateTask model.task }, Cmd.none )

        ValidateTask ->
            ( { model | error = validateTask model.task }, Cmd.none )

        SubmitForm ->
            case validateTask model.task of
                Just error ->
                    ( { model | error = Just error }, Cmd.none )

                Nothing ->
                    ( { model | task = "", tasks = List.append model.tasks [ createTask model.task ] }, Cmd.none )

        InputCompleteToggle task completed ->
            ( { model
                | tasks =
                    List.foldl
                        (\cTask tasks ->
                            if task == cTask then
                                { cTask
                                    | completedAt =
                                        if completed then
                                            Just model.time

                                        else
                                            Nothing
                                }
                                    :: tasks

                            else
                                cTask :: tasks
                        )
                        []
                        model.tasks
              }
            , Cmd.none
            )


viewErrorMessage : Maybe String -> Html msg
viewErrorMessage error =
    case error of
        Just err ->
            div [] [ text err ]

        Nothing ->
            div [] []


view : Model -> Browser.Document Msg
view model =
    { title = "fml"
    , body =
        [ form [ onSubmit SubmitForm ]
            [ div []
                [ input
                    [ placeholder "What needs to be done?"
                    , onInput InputTask
                    , onBlur ValidateTask
                    , value model.task
                    ]
                    []
                , button [ type_ "submit" ] [ text "Add" ]
                ]
            , viewErrorMessage model.error
            ]
        , ul []
            (List.map
                (\task ->
                    li []
                        [ label []
                            [ input [ type_ "checkbox", onCheck (InputCompleteToggle task) ] []
                            , text task.task
                            , text
                                (case task.completedAt of
                                    Just time ->
                                        showTime Time.utc time

                                    Nothing ->
                                        "-"
                                )
                            ]
                        ]
                )
                model.tasks
            )
        ]
    }


showTime : Time.Zone -> Time.Posix -> String
showTime zone time =
    let
        timePieces =
            List.intersperse ":" (List.map (\fn -> fn zone time |> String.fromInt) [ Time.toHour, Time.toMinute, Time.toSecond ])

        datePieces =
            List.intersperse "/" (List.map (\fn -> fn zone time |> String.fromInt) [ Time.toDay, Time.toMonth, Time.toYear ])
    in
    String.join " " (List.map (String.concat) [ datePieces, timePieces ])


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions

        -- , onUrlChange = onUrlChange
        -- , onUrlRequest = onUrlRequest
        }
