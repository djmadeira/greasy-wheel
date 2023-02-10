port module Main exposing (Model, Msg(..), init, main, setReflexes, toJs, update, view)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, at, decodeString, field, int, keyValuePairs, map2, map5, maybe, string)
import List exposing (drop, filter, foldl, head, length, repeat, tail, take)
import List.Extra exposing (getAt, groupWhile, splitAt, takeWhile, unfoldr)
import OrderedDict
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
    , id : String
    , name : String
    , cost : Maybe Int
    , caution : Maybe String
    }


type alias Volley =
    { actionsPerVolley : Int, actions : OrderedDict.OrderedDict String Action }


type VolleyIndex
    = First
    | Second
    | Third


type alias ActionQueue =
    { first : List String, second : List String, third : List String }


initActionQueue : ActionQueue
initActionQueue =
    { first = [], second = [], third = [] }


type alias Model =
    { reflexes : Int
    , spentActions : Int
    , actionQueue : ActionQueue
    , decodeError : String
    , serverMessage : String
    , fight : Volley
    }


type alias ActionDat =
    { group : String
    , actionType : String
    , name : String
    , cost : Maybe Int
    , caution : Maybe String
    }


actionDatToAction : String -> ActionDat -> Action
actionDatToAction id data =
    { group = data.group
    , actionType = data.actionType
    , id = id
    , name = data.name
    , cost = data.cost
    , caution = data.caution
    }


actionDatDecoder : Decoder ActionDat
actionDatDecoder =
    map5 ActionDat
        (field "group" string)
        (field "type" string)
        (field "name" string)
        (maybe (field "cost" int))
        (maybe (field "caution" string))


orderedDict : Decoder a -> Decoder (OrderedDict.OrderedDict String a)
orderedDict decoder =
    Decode.map OrderedDict.fromList (keyValuePairs decoder)


volleyDecoder : Decoder Volley
volleyDecoder =
    at [ "fight" ]
        (map2 Volley
            (field "actionsPerVolley" int)
            (field "actions" (Decode.map (OrderedDict.map actionDatToAction) (orderedDict actionDatDecoder)))
        )


loadActions : String -> ( Volley, String )
loadActions json =
    let
        res =
            decodeString volleyDecoder json
    in
    case res of
        Err e ->
            ( { actionsPerVolley = 0, actions = OrderedDict.empty }, Decode.errorToString e )

        Ok a ->
            ( a, "" )


init : String -> ( Model, Cmd Msg )
init flags =
    let
        decodeResult =
            loadActions flags
    in
    ( { reflexes = 0
      , spentActions = 0
      , actionQueue = initActionQueue
      , serverMessage = ""
      , decodeError = Tuple.second decodeResult
      , fight = Tuple.first decodeResult
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
    | UnqueueAction String VolleyIndex Int
    | ClearQueue
    | TestServer
    | OnServerResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        QueueAction action volley cost ->
            ( queueAction model action volley cost, toJs "QueueAction" )

        UnqueueAction action volley cost ->
            ( unqueueAction model action volley cost, toJs "QueueAction" )

        ClearQueue ->
            ( { model | actionQueue = initActionQueue, spentActions = 0 }, toJs "ClearQueue" )

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
    { model | reflexes = r, spentActions = 0, actionQueue = { first = [], second = [], third = [] } }


unqueueAction : Model -> String -> VolleyIndex -> Int -> Model
unqueueAction model action index cost =
    let
        newQueue actionQueue =
            case index of
                First ->
                    { actionQueue | first = filter (\a -> a /= action) model.actionQueue.first }

                Second ->
                    { actionQueue | second = filter (\a -> a /= action) model.actionQueue.second }

                Third ->
                    { actionQueue | third = filter (\a -> a /= action) model.actionQueue.third }
    in
    { model | spentActions = model.spentActions - cost, actionQueue = newQueue model.actionQueue }


queueAction : Model -> String -> VolleyIndex -> Int -> Model
queueAction model action index cost =
    let
        newQueue actionQueue =
            case index of
                First ->
                    { actionQueue | first = model.actionQueue.first ++ [ action ] }

                Second ->
                    { actionQueue | second = model.actionQueue.second ++ [ action ] }

                Third ->
                    { actionQueue | third = model.actionQueue.third ++ [ action ] }
    in
    { model | spentActions = model.spentActions + cost, actionQueue = newQueue model.actionQueue }



-- ---------------------------
-- VIEW
-- ---------------------------


viewActionQueue : String -> List String -> Html Msg
viewActionQueue label actions =
    div [ class "action-queue--volley" ]
        [ h3 [] [ text <| "Volley " ++ label ]
        , span [] [ text <| String.join ", " actions ]
        ]


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ header [] []
        , main_ []
            [ div [ class "inputs" ]
                [ label [ class "inputs--reflexes" ] [ span [] [ text "Reflexes B" ], input [ onInput SetReflexes ] [] ]
                ]
            , div [ class "outputs" ]
                [ div [ class "available-actions" ]
                    [ h2 [] [ text "Spent Actions" ]
                    , span [] [ text <| String.fromInt model.spentActions ]
                    ]
                , div [ class "action-queue" ]
                    [ h2 [] [ text "Selected Actions", button [ class "action-queue--clear", onClick ClearQueue ] [ text "Clear" ] ]
                    , div [ class "action-queue--wrap" ]
                        [ viewActionQueue "1" model.actionQueue.first
                        , viewActionQueue "2" model.actionQueue.second
                        , viewActionQueue "3" model.actionQueue.third
                        ]
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
                (List.map (\index -> viewVolley model index) [ First, Second, Third ])

            -- )
            ]
        ]


tooltip : String -> String -> Html Msg
tooltip anchor content =
    span [ class "tooltip--anchor" ] [ text anchor, div [ class "tooltip--content" ] [ text content ] ]


notEmpty : List (List String) -> Maybe ( List String, List (List String) )
notEmpty actions =
    let
        h =
            List.concat <| List.map (take 1) actions

        t =
            List.map (drop 1) actions
    in
    if length h < 1 then
        Nothing

    else
        Just ( h, t )


actionsToRounds : List (List String) -> List (List String)
actionsToRounds actions =
    let
        rounds =
            unfoldr notEmpty actions

        roundsSize =
            length rounds

        lastRoundSize =
            case tail rounds of
                Just x ->
                    length x

                Nothing ->
                    0
    in
    if roundsSize == 0 || lastRoundSize == 3 then
        rounds ++ [ [] ]

    else
        rounds


queueFromIndex : Model -> VolleyIndex -> List String
queueFromIndex model index =
    case index of
        First ->
            model.actionQueue.first

        Second ->
            model.actionQueue.second

        Third ->
            model.actionQueue.third


actionButton : Model -> VolleyIndex -> Action -> List String -> Html Msg
actionButton model index action round =
    let
        queue =
            queueFromIndex model index

        actionChecked =
            List.member action.id round && List.member action.id queue

        buttonDisabled =
            if actionChecked then
                False

            else if not <| List.isEmpty queue then
                True

            else
                False

        className =
            if actionChecked then
                "selected"

            else
                ""

        buttonLabel =
            if actionChecked then
                "X"

            else
                case action.cost of
                    Just x ->
                        if x == 1 then
                            String.fromChar noBreakSpace

                        else
                            String.fromInt x

                    Nothing ->
                        case action.actionType of
                            "Variable" ->
                                "x"

                            _ ->
                                String.fromChar noBreakSpace

        clickHandler =
            if actionChecked then
                UnqueueAction

            else
                QueueAction

        buttonAttrs =
            if buttonDisabled then
                [ disabled True ]

            else
                [ onClick
                    (clickHandler action.id
                        index
                        (case action.cost of
                            Just x ->
                                x

                            Nothing ->
                                0
                        )
                    )
                ]
    in
    button (class className :: buttonAttrs) [ text buttonLabel ]


viewAction : Model -> VolleyIndex -> Action -> Html Msg
viewAction model index action =
    let
        rounds =
            actionsToRounds [ model.actionQueue.first, model.actionQueue.second, model.actionQueue.third ]

        test =
            Debug.log "test" <| Debug.toString rounds

        cautionTooltip =
            case action.caution of
                Nothing ->
                    span [] []

                Just v ->
                    span [] [ tooltip "*" v ]
    in
    div [ class "volley--action" ]
        (List.map (actionButton model index action) rounds
            ++ [ span [] [ text action.name ]
               , cautionTooltip
               ]
        )


viewVolley : Model -> VolleyIndex -> Html Msg
viewVolley model index =
    let
        viewActionGroup ( firstMember, otherMembers ) =
            div [ class "volley--action-group" ]
                [ h3 [ class "volley--action-group-header" ] [ text (firstMember.group ++ " Actions") ]
                , div [ class "volley--action-group-actions" ] (List.map (viewAction model index) (firstMember :: otherMembers))
                ]

        n =
            case index of
                First ->
                    "1"

                Second ->
                    "2"

                Third ->
                    "3"
    in
    div [ class "volley" ]
        (h2 [] [ text <| "Volley " ++ n ]
            :: List.map viewActionGroup (groupWhile (\a b -> a.group == b.group) (OrderedDict.values model.fight.actions))
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
