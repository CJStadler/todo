port module Main exposing (Flags, Model, Msg(..), SerializedModel, emptyModel, infoFooter, init, initModel, main, serialize, setStorage, update, updateWithStorage, view, viewHeader)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

1.  Model - a full definition of the application's state
2.  Update - a way to step the application state forward
3.  View - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://gnextIde.elm-lang.org/architecture/index.html>

-}

import Browser
import Component.EntryList as EntryList
import Date exposing (Date)
import Entry exposing (Entry)
import Helpers exposing (onEnter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Json.Decode as Json
import Maybe
import Task


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • TodoMVC", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : SerializedModel -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage (serialize newModel), cmds ]
    )


type alias SerializedModel =
    { entries : List Entry.Serialized
    , field : String
    , nextId : Int
    , visibility : String
    }


serialize : Model -> SerializedModel
serialize model =
    { entries = List.map Entry.serialize model.entries
    , field = model.field
    , nextId = model.nextId
    , visibility = EntryList.visibilityText model.listState
    }


initModel : Flags -> Model
initModel flags =
    let
        serialized =
            Maybe.withDefault emptyModel flags.model
    in
    { entries = List.map Entry.deserialize serialized.entries
    , field = serialized.field
    , nextId = serialized.nextId
    , currentDate = Date.fromRataDie 0 -- TODO: Handle this better.
    , listState = EntryList.init serialized.visibility
    }



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { entries : List Entry
    , field : String
    , nextId : Entry.Id
    , listState : EntryList.Model
    , currentDate : Date
    }


emptyModel : SerializedModel
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , nextId = 0
    }


type alias Flags =
    { model : Maybe SerializedModel }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags
    , Task.perform SetDate Date.today
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | SetDate Date
    | UpdateField String
    | Add
    | UpdateEntry Entry.Id String
    | DeleteEntry Entry.Id
    | DeleteComplete Date
    | CheckEntry Entry.Id Bool
    | CheckAll Date Bool
    | EntryListMsg EntryList.InternalMsg



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetDate date ->
            ( { model | currentDate = date }
            , Cmd.none
            )

        Add ->
            ( { model
                | nextId = model.nextId + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        model.entries
                            ++ [ Entry.new model.field
                                    model.nextId
                                    model.currentDate
                               ]
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        UpdateEntry id desc ->
            let
                updateById entry =
                    if id == Entry.id entry then
                        Entry.update (Entry.Description desc) entry

                    else
                        entry
            in
            ( { model | entries = List.map updateById model.entries }
            , Cmd.none
            )

        DeleteEntry id ->
            ( { model | entries = List.filter (\t -> Entry.id t /= id) model.entries }
            , Cmd.none
            )

        DeleteComplete date ->
            let
                completedOnDate entry =
                    Entry.completed entry && Entry.date entry == date
            in
            ( { model | entries = List.filter (not << completedOnDate) model.entries }
            , Cmd.none
            )

        CheckEntry id isCompleted ->
            let
                updateEntry t =
                    if Entry.id t == id then
                        Entry.update (Entry.Completed isCompleted) t

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        CheckAll date isCompleted ->
            let
                updateEntry t =
                    if Entry.date t == date then
                        Entry.update (Entry.Completed isCompleted) t

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        EntryListMsg m ->
            let
                ( newState, cmd ) =
                    EntryList.update m model.listState
            in
            ( { model | listState = newState }
            , Cmd.map EntryListMsg cmd
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            [ class "todoapp" ]
            [ lazy2 viewHeader model.currentDate model.field
            , lazy viewEntryList model
            ]
        , infoFooter
        ]


viewEntryList : Model -> Html Msg
viewEntryList model =
    let
        entries =
            List.filter (Entry.onDate model.currentDate) model.entries

        config =
            { check = CheckEntry
            , checkAll = CheckAll model.currentDate
            , updateEntry = UpdateEntry
            , delete = DeleteEntry
            , deleteComplete = DeleteComplete model.currentDate
            }

        html =
            EntryList.view config model.listState entries

        toMsg msg =
            case msg of
                EntryList.External m ->
                    m

                EntryList.Internal m ->
                    EntryListMsg m
    in
    Html.map toMsg html


viewHeader : Date -> String -> Html Msg
viewHeader currentDate todoStr =
    let
        dateString =
            Date.format "MMM ddd, y" currentDate

        previousDay =
            SetDate (Date.add Date.Days -1 currentDate)

        nextDay =
            SetDate (Date.add Date.Days 1 currentDate)
    in
    header
        [ class "header" ]
        [ h1 [] [ text dateString ]
        , div []
            [ button [ onClick previousDay ] [ text "Previous" ]
            , button [ onClick nextDay ] [ text "Next" ]
            ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value todoStr
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]



-- VIEW ALL ENTRIES


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
