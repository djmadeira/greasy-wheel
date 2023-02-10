port module Main exposing (Model, Msg(..), init, main, setReflexes, toJs, update, view)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, at, decodeString, dict, field, int, map2, map5, nullable, string)
import List.Extra exposing (groupWhile)
import SpecialChars exposing (noBreakSpace)



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Action =
    { group : String
    , actionType : String
    , name : String
    , cost : Int
    , caution : Maybe String
    }


type alias Volley =
    { actionsPerVolley : Int, actions : Dict.Dict String Action }


type VolleyIndex
    = Volley1
    | Volley2
    | Volley3


type alias ActionQueue =
    { volley1 : List String, volley2 : List String, volley3 : List String }


initActionQueue : ActionQueue
initActionQueue =
    { volley1 = [], volley2 = [], volley3 = [] }


type alias Model =
    { reflexes : Int
    , spentActions : Int
    , actionQueue : ActionQueue
    , serverMessage : String
    , fight : Volley
    }


type alias ActionDat =
    { group : String
    , actionType : String
    , name : String
    , cost : Int
    , caution : Maybe String
    }


actionDatToAction : String -> ActionDat -> Action
actionDatToAction id data =
    { group = data.group
    , actionType = data.actionType
    , name = id
    , cost = data.cost
    , caution = data.caution
    }


actionDatDecoder : Decoder ActionDat
actionDatDecoder =
    map5 ActionDat
        (field "group" string)
        (field "type" string)
        (field "name" string)
        (field "cost" int)
        (field "caution" <| nullable string)


volleyDecoder : Decoder Volley
volleyDecoder =
    at [ "fight" ]
        (map2 Volley
            (field "actionsPerVolley" int)
            (field "actions" (Decode.map (Dict.map actionDatToAction) (dict actionDatDecoder)))
        )


loadActions : String -> Volley
loadActions json =
    let
        res =
            decodeString volleyDecoder json
    in
    case res of
        Err _ ->
            { actionsPerVolley = 0, actions = Dict.empty }

        Ok a ->
            a


init : String -> ( Model, Cmd Msg )
init flags =
    ( { reflexes = 0
      , spentActions = 0
      , actionQueue = initActionQueue
      , serverMessage = ""
      , fight = loadActions flags
      }
    , Cmd.none
    )



-- }
-- ]
--       }
-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = SetReflexes String
    | QueueAction String VolleyIndex Int
    | ClearQueue
    | TestServer
    | OnServerResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        QueueAction action volley cost ->
            ( queueAction model action volley cost, toJs "QueueAction" )

        ClearQueue ->
            ( { model | actionQueue = initActionQueue }, toJs "ClearQueue" )

        SetReflexes input ->
            let
                r =
                    String.toInt input
            in
            case r of
                Nothing ->
                    ( setReflexes model 0, toJs "SetReflexes" )

                Just n ->
                    ( setReflexes model n, toJs "SetReflexes" )

        TestServer ->
            let
                expect =
                    Http.expectJson OnServerResponse (Decode.field "result" Decode.string)
            in
            ( model
            , Http.get { url = "/test", expect = expect }
            )

        OnServerResponse res ->
            case res of
                Ok r ->
                    ( { model | serverMessage = r }, Cmd.none )

                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadUrl url ->
            "BadUrl: " ++ url

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus _ ->
            "BadStatus"

        BadBody s ->
            "BadBody: " ++ s


{-| increments the counter

    add1 5 --> 6

-}
setReflexes : Model -> Int -> Model
setReflexes model r =
    { model | reflexes = r, spentActions = 0, actionQueue = { volley1 = [], volley2 = [], volley3 = [] } }


queueAction : Model -> String -> VolleyIndex -> Int -> Model
queueAction model action index cost =
    let
        newQueue actionQueue =
            case index of
                Volley1 ->
                    { actionQueue | volley1 = model.actionQueue.volley1 ++ [ action ] }

                Volley2 ->
                    { actionQueue | volley2 = model.actionQueue.volley2 ++ [ action ] }

                Volley3 ->
                    { actionQueue | volley3 = model.actionQueue.volley3 ++ [ action ] }
    in
    { model | spentActions = model.spentActions - cost, actionQueue = newQueue model.actionQueue }



-- ---------------------------
-- VIEW
-- ---------------------------


viewActionQueue : String -> List String -> Html Msg
viewActionQueue label actions =
    div [ class "action-queue--volley" ] <|
        List.map (\action -> span [] [ text action ]) actions


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ header [] []
        , main_ []
            [ div [ class "inputs" ]
                [ label [ class "inputs--reflexes" ] [ span [] [ text "Reflexes" ], input [ onInput SetReflexes ] [] ]
                ]
            , div [ class "outputs" ]
                [ div [ class "action-queue" ]
                    [ h2 [] [ text "Selected Actions", button [ class "action-queue--clear", onClick ClearQueue ] [ text "Clear" ] ]
                    , viewActionQueue "1" model.actionQueue.volley1
                    , viewActionQueue "2" model.actionQueue.volley2
                    , viewActionQueue "3" model.actionQueue.volley3
                    ]
                ]
            , div [ class "exchange" ]
                -- (List.append
                --     [ div [ class "volley appendix" ]
                --         [ h2 [ class "volley--appendix-header" ] [ text "Disadvantage to all actions except Defense actions" ]
                --         , div [ class "volley--placeholder large" ] [ text "-" ]
                --         , div [ class "volley--placeholder large" ] [ text "+1 Ob" ]
                --         , div [ class "volley--placeholder large" ] [ text "+2 Ob" ]
                --         , div [ class "volley--placeholder large" ] [ text "+3 Ob" ]
                --         , div [ class "volley--placeholder large" ] [ text "+4 Ob" ]
                --         , div [ class "volley--placeholder large" ] [ text "+5 Ob" ]
                --         ]
                --     ]
                (List.map (\index -> viewVolley model index) [ Volley1, Volley2, Volley3 ])

            -- )
            ]
        ]


tooltip : String -> String -> Html Msg
tooltip anchor content =
    span [ class "tooltip--anchor" ] [ text anchor, div [ class "tooltip--content" ] [ text content ] ]


viewVolley : Model -> VolleyIndex -> Html Msg
viewVolley model index =
    let
        viewActionGroup ( { group }, actions ) =
            div [ class "volley--action-group" ]
                [ h3 [ class "volley--action-group-header" ] [ text (group ++ " Actions") ]
                , div [ class "volley--action-group-actions" ] (List.map viewAction actions)
                ]

        buttonLabel cost =
            if cost == 1 then
                String.fromChar noBreakSpace

            else
                String.fromInt cost

        clickHandler action cost =
            onClick <| QueueAction action index cost

        cautionTooltip name caution =
            case caution of
                Nothing ->
                    span [] []

                Just v ->
                    span []
                        [ text name
                        , span [] [ tooltip "*" v ]
                        ]

        viewAction { actionType, name, cost, caution } =
            div [ class "volley--action" ]
                (case actionType of
                    "Variable" ->
                        [ input [ placeholder "x" ] [], span [] [ text name ] ]

                    _ ->
                        [ button [ clickHandler name cost ] [ text <| buttonLabel cost ]
                        , cautionTooltip name caution
                        ]
                )

        n =
            case index of
                Volley1 ->
                    "1"

                Volley2 ->
                    "2"

                Volley3 ->
                    "3"
    in
    div [ class "volley" ]
        (List.append [ h2 [] [ text <| "Volley " ++ n ] ]
            (List.map viewActionGroup (groupWhile (\a b -> a.group == b.group) (Dict.values model.fight.actions)))
        )



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "[title] Elm 0.19.1 starter"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
