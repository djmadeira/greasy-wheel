port module Main exposing (Model, Msg(..), add1, init, main, toJs, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Action =
    { name : String
    , cost : Int
    , addlReq : String
    }


type alias ActionGroup =
    { name : String
    , actions : List Action
    }


type alias Volley =
    { actionsPerVolley : Int
    , actionGroups : List ActionGroup
    }


type alias Model =
    { counter : Int
    , serverMessage : String
    , fight : Volley
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { counter = flags
      , serverMessage = ""
      , fight =
            { actionsPerVolley = 3
            , actionGroups =
                [ { name = "Attack"
                  , actions =
                        [ { name = "Strike", cost = 1, addlReq = "" }
                        , { name = "Great Strike", cost = 2, addlReq = "" }
                        , { name = "Block & Strike", cost = 1, addlReq = "Requires Shield Training" }
                        , { name = "Lock & Strike", cost = 1, addlReq = "Requires a special trait like Wolf Snout" }
                        ]
                  }
                , { name = "Defense"
                  , actions =
                        [ { name = "Avoid", cost = 1, addlReq = "" }
                        , { name = "Block", cost = 1, addlReq = "" }
                        , { name = "Counterstrike", cost = 1, addlReq = "" }
                        ]
                  }
                , { name = "Basic Fighting"
                  , actions =
                        [ { name = "Assess", cost = 1, addlReq = "" }
                        , { name = "Change Stance", cost = 1, addlReq = "" }
                        , { name = "Charge/Tackle", cost = 1, addlReq = "" }
                        , { name = "Draw Weapon", cost = 2, addlReq = "" }
                        , { name = "Get Up", cost = 2, addlReq = "" }
                        , { name = "Lock", cost = 1, addlReq = "" }
                        , { name = "Push", cost = 1, addlReq = "" }
                        , { name = "Physical Action", cost = 2, addlReq = "" }
                        ]
                  }
                , { name = "Special Fighting"
                  , actions =
                        [ { name = "Beat", cost = 1, addlReq = "" }
                        , { name = "Disarm", cost = 1, addlReq = "" }
                        , { name = "Feint", cost = 1, addlReq = "" }
                        , { name = "Throw Person", cost = 1, addlReq = "" }
                        ]
                  }
                , { name = "Shooting and Throwing"
                  , actions =
                        [ { name = "Aim", cost = 1, addlReq = "" }
                        , { name = "Fire Gun/Crossbow", cost = 2, addlReq = "" }
                        , { name = "Nock and Draw", cost = 5, addlReq = "" }
                        , { name = "Release Bow", cost = 1, addlReq = "" }
                        , { name = "Snapshot", cost = 1, addlReq = "" }
                        , { name = "Throw Weapon", cost = 2, addlReq = "" }
                        ]
                  }
                , { name = "Magic"
                  , actions =
                        [ { name = "Cast Spell", cost = -1, addlReq = "Cost is determined by spell" }
                        , { name = "Drop Spell", cost = 1, addlReq = "" }
                        , { name = "Command Spirit", cost = 1, addlReq = "" }
                        , { name = "Sing, Howl, Pray", cost = -1, addlReq = "Does not cost actions. Listed on the action sheet for timing purposes only." }
                        ]
                  }
                , { name = "Social"
                  , actions =
                        [ { name = "Command", cost = 2, addlReq = "" }
                        , { name = "Intimidate", cost = 2, addlReq = "" }
                        ]
                  }
                , { name = "Hesitation"
                  , actions =
                        [ { name = "Fall Prone", cost = 1, addlReq = "" }
                        , { name = "Run Screaming", cost = 1, addlReq = "" }
                        , { name = "Stand and Drool", cost = 1, addlReq = "" }
                        , { name = "Swoon", cost = 1, addlReq = "" }
                        ]
                  }
                ]
            }
      }
    , Cmd.none
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Inc
    | TestServer
    | OnServerResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Inc ->
            ( add1 model, toJs "Inc" )

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
add1 : Model -> Model
add1 model =
    { model | counter = model.counter + 1 }



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ header [] []
        , main_ []
            [ div [ class "engagement" ]
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
                (viewEngagement model.fight)

            -- )
            ]
        ]


viewEngagement : Volley -> List (Html Msg)
viewEngagement volley =
    List.map (viewVolley volley) [ 1, 2, 3 ]


actionButton : Action -> Html Msg
actionButton action =
    let
        actionPlaceholder =
            if action.addlReq == "" then
                if action.cost == 0 then
                    "x"

                else if action.cost == -1 then
                    "§"

                else
                    String.fromInt action.cost

            else
                "*"

        attributes =
            if action.addlReq == "" then
                []

            else
                [ title action.addlReq ]
    in
    button attributes [ text actionPlaceholder ]


viewVolley : Volley -> Int -> Html Msg
viewVolley volley n =
    let
        viewActionGroup actionGroup =
            div [ class "volley--action-group" ]
                [ h3 [ class "volley--action-group-header" ] [ text (actionGroup.name ++ " Actions") ]
                , div [ class "volley--action-group-actions" ] (List.map viewAction actionGroup.actions)
                ]

        viewAction action =
            div [ class "volley--action" ] (List.append (List.repeat volley.actionsPerVolley (actionButton action)) [ span [] [ text action.name ] ])
    in
    div [ class "volley" ]
        (List.append [ h2 [] [ text <| "Volley " ++ String.fromInt n ] ] (List.map viewActionGroup volley.actionGroups))



-- div [ class "container p-2" ]
--     [ header [ class "grid-cols-3" ]
--         [ span [] [ img [ src "/images/logo.png" ] [] ]
--         , div [] [ span [ class "icon" ] [] ]
--         , h1 [ class "text-2xl font-bold ml-2" ] [ text "Elm 0.19.1 Webpack Starter, with hot-reloading" ]
--         ]
--     , p [] [ text "Click on the button below to increment the state." ]
--     , div [ class "flex flex-row justify-between" ]
--         [ div [ class "flex flex-row items-center" ]
--             [ button
--                 [ class "border border-green-500 bg-green-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-green-600 focus:outline-none focus:shadow-outline"
--                 , onClick Inc
--                 ]
--                 [ text "+ 1" ]
--             , text <| String.fromInt model.counter
--             ]
--         , div [ class "flex flex-row items-center" ]
--             [ text model.serverMessage
--             , button
--                 [ class "border border-green-500 bg-green-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-green-600 focus:outline-none focus:shadow-outline"
--                 , onClick TestServer
--                 ]
--                 [ text "ping dev server" ]
--             ]
--         ]
--     , p [] [ text "Then make a change to the source code and see how the state is retained after recompilation." ]
--     ]
--     [
--         ]
-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
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
