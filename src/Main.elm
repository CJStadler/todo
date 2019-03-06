port module Main exposing (Flags, Model, Msg(..), SerializedModel, emptyModel, infoFooter, init, initModel, main, serialize, setStorage, update, updateWithStorage, view)

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
import EntrySchedule exposing (EntrySchedule)
import Helpers exposing (onEnter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ImportEntries exposing (filter, update)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe
import Task


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "TODO", body = [ view model ] }
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



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { schedules : List EntrySchedule
    , field : String
    , nextId : EntrySchedule.Id
    , listState : EntryList.Model
    , activeDate : Date
    , todayDate : Maybe Date
    , lastOpenedDate : Date -- The last date the application was opened.
    }


emptyModel : SerializedModel
emptyModel =
    { schedules = []
    , visibility = "All"
    , field = ""
    , nextId = 0
    , lastOpenedRataDie = 0
    }


type alias SerializedModel =
    { schedules : List Encode.Value
    , field : String
    , nextId : Int
    , visibility : String
    , lastOpenedRataDie : Int
    }


serialize : Model -> SerializedModel
serialize model =
    { schedules = List.map EntrySchedule.encode model.schedules
    , field = model.field
    , nextId = model.nextId
    , visibility = EntryList.visibilityText model.listState
    , lastOpenedRataDie = Date.toRataDie model.lastOpenedDate
    }


initModel : Flags -> Model
initModel flags =
    let
        serialized =
            Maybe.withDefault emptyModel flags.model

        decode val =
            case Decode.decodeValue EntrySchedule.decode val of
                Ok schedule ->
                    Just schedule

                Err msg ->
                    let
                        _ =
                            Debug.log (Decode.errorToString msg ++ "in") (Encode.encode 0 val)
                    in
                    Nothing

        schedules =
            List.map decode serialized.schedules
                |> List.filterMap (\x -> x)
    in
    { schedules = schedules
    , field = serialized.field
    , nextId = serialized.nextId
    , activeDate = Date.fromRataDie 0 -- TODO: Handle this better.
    , todayDate = Maybe.Nothing
    , lastOpenedDate = Date.fromRataDie serialized.lastOpenedRataDie
    , listState = EntryList.init serialized.visibility
    }


type alias Flags =
    { model : Maybe SerializedModel }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags
    , Task.perform SetToday Date.today
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | SetToday Date
    | SetDate Date
    | ImportPrevious Bool
    | UpdateField String
    | Add
    | UpdateEntry EntrySchedule.Id String
    | DeleteEntry EntrySchedule.Id
    | CheckEntry Date EntrySchedule.Id Bool
    | CheckAll Date Bool
    | EntryListMsg EntryList.InternalMsg



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        SetToday date ->
            let
                updatedModel =
                    setActiveDate model date
            in
            ( { updatedModel | todayDate = Maybe.Just date }
            , Cmd.none
            )

        SetDate date ->
            ( setActiveDate model date
            , Cmd.none
            )

        ImportPrevious shouldImport ->
            let
                updated =
                    case model.todayDate of
                        Just today ->
                            let
                                ( nextId, updatedSchedules ) =
                                    if shouldImport then
                                        ImportEntries.update
                                            model.lastOpenedDate
                                            today
                                            model.nextId
                                            model.schedules

                                    else
                                        ( model.nextId, model.schedules )
                            in
                            { model
                                | lastOpenedDate = today
                                , nextId = nextId
                                , schedules = updatedSchedules
                            }

                        Nothing ->
                            model
            in
            ( updated
            , Cmd.none
            )

        Add ->
            ( { model
                | nextId = model.nextId + 1
                , field = ""
                , schedules =
                    if String.isEmpty model.field then
                        model.schedules

                    else
                        model.schedules
                            ++ [ EntrySchedule.newSingle model.nextId
                                    model.field
                                    model.activeDate
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
                updateById schedule =
                    if id == EntrySchedule.getId schedule then
                        EntrySchedule.edit desc schedule

                    else
                        schedule
            in
            ( { model | schedules = List.map updateById model.schedules }
            , Cmd.none
            )

        DeleteEntry id ->
            ( { model
                | schedules =
                    List.filter (\t -> EntrySchedule.getId t /= id)
                        model.schedules
              }
            , Cmd.none
            )

        CheckEntry date id isCompleted ->
            let
                completeEntry schedule =
                    if EntrySchedule.getId schedule == id then
                        EntrySchedule.complete date isCompleted schedule

                    else
                        schedule
            in
            ( { model | schedules = List.map completeEntry model.schedules }
            , Cmd.none
            )

        CheckAll date isCompleted ->
            let
                updateSchedule schedule =
                    EntrySchedule.complete date isCompleted schedule
            in
            ( { model | schedules = List.map updateSchedule model.schedules }
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


setActiveDate : Model -> Date -> Model
setActiveDate model date =
    { model | activeDate = date }



-- VIEW


view : Model -> Html Msg
view model =
    let
        importPrompt =
            case model.todayDate of
                Just d ->
                    viewImportPrompt model.lastOpenedDate d model.schedules

                Nothing ->
                    div [] []
    in
    div
        [ class "todomvc-wrapper" ]
        [ section
            [ class "todoapp" ]
            [ viewDate model.activeDate
            , importPrompt
            , div [ class "panel" ]
                [ newEntryInput model.field
                , viewEntryList model
                ]
            ]
        , infoFooter
        ]


viewDate : Date -> Html Msg
viewDate activeDate =
    let
        dateString =
            Date.format "MMM ddd, y" activeDate

        previousDay =
            SetDate (Date.add Date.Days -1 activeDate)

        nextDay =
            SetDate (Date.add Date.Days 1 activeDate)
    in
    div [ id "date-display" ]
        [ div [ id "date-header" ] [ h1 [] [ text dateString ] ]
        , div [ id "date-buttons" ]
            [ button [ id "prev-day", onClick previousDay ] [ text "❮" ]
            , button [ id "next-day", onClick nextDay ] [ text "❯" ]
            ]
        ]


newEntryInput : String -> Html Msg
newEntryInput inputVal =
    input
        [ class "new-todo"
        , placeholder "What needs to be done?"
        , autofocus True
        , value inputVal
        , name "newTodo"
        , onInput UpdateField
        , onEnter Add
        ]
        []


entriesOnDate : Date -> List EntrySchedule -> List Entry
entriesOnDate date schedules =
    List.filterMap (EntrySchedule.forDate date) schedules


viewEntryList : Model -> Html Msg
viewEntryList model =
    let
        entries =
            entriesOnDate model.activeDate model.schedules

        config =
            { check = CheckEntry model.activeDate
            , checkAll = CheckAll model.activeDate
            , updateEntry = UpdateEntry
            , delete = DeleteEntry
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


viewImportPrompt : Date -> Date -> List EntrySchedule -> Html Msg
viewImportPrompt lastOpened today schedules =
    let
        entryCount =
            ImportEntries.filter lastOpened today schedules
                |> List.length
                |> Debug.log "entries"
    in
    if entryCount == 0 then
        text ""
        -- Empty node.

    else
        div [ class "panel", id "import-prompt" ]
            [ p [] [ text (promptString lastOpened entryCount) ]
            , button [ onClick (ImportPrevious True) ] [ text "Yes" ]
            , button [ onClick (ImportPrevious False) ] [ text "No" ]
            ]


promptString : Date -> Int -> String
promptString from count =
    "Import incomplete entries since "
        ++ Date.format "MM/dd/yy" from
        ++ " ("
        ++ String.fromInt count
        ++ " entries)?"


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Get the "
            , a [ href "https://github.com/CJStadler/todo" ] [ text "code" ]
            ]
        , p []
            [ text "Based on "
            , a [ href "https://github.com/evancz/elm-todomvc" ] [ text "Elm TodoMVC" ]
            ]
        ]
