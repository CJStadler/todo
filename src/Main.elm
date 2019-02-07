port module Main exposing (Flags, Model, Msg(..), SerializedModel, Visibility(..), emptyModel, infoFooter, init, initModel, main, onEnter, serialize, setStorage, update, updateWithStorage, view, viewControls, viewControlsClear, viewControlsCount, viewControlsFilters, viewEntries, viewEntry, viewHeader, viewKeyedEntry, visibilitySwap, visibilityText)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

1.  Model - a full definition of the application's state
2.  Update - a way to step the application state forward
3.  View - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>

-}

import Browser
import Browser.Dom as Dom
import Date exposing (Date)
import Entry exposing (Entry)
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
    , uid : Int
    , visibility : String
    }


serialize : Model -> SerializedModel
serialize model =
    { entries = List.map Entry.serialize model.entries
    , field = model.field
    , uid = model.uid
    , visibility = visibilityText model.visibility
    }


initModel : Flags -> Model
initModel flags =
    let
        serialized =
            Maybe.withDefault emptyModel flags.model

        viz =
            if visibilityText Active == serialized.visibility then
                Active

            else if visibilityText Completed == serialized.visibility then
                Completed

            else
                All
    in
    { entries = List.map Entry.deserialize serialized.entries
    , field = serialized.field
    , uid = serialized.uid
    , visibility = viz
    , editingId = Maybe.Nothing
    , currentDate = Date.fromRataDie 0 -- TODO: Handle this better.
    }



-- MODEL


type Visibility
    = All
    | Active
    | Completed


visibilityText : Visibility -> String
visibilityText visibility =
    case visibility of
        All ->
            "All"

        Active ->
            "Active"

        Completed ->
            "Done"



-- The full application state of our todo app.


type alias EntryId =
    -- TODO: Move to Entry module
    Int


type alias Model =
    { entries : List Entry
    , field : String
    , uid : EntryId
    , visibility : Visibility
    , editingId : Maybe EntryId
    , currentDate : Date
    }


emptyModel : SerializedModel
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , uid = 0
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
    | EditingEntry Int
    | FinishEdit
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility Visibility



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
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        model.entries
                            ++ [ Entry.new model.field
                                    model.uid
                                    model.currentDate
                               ]
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        EditingEntry id ->
            let
                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | editingId = Maybe.Just id }
            , Task.attempt (\_ -> NoOp) focus
            )

        FinishEdit ->
            ( { model | editingId = Maybe.Nothing }
            , Cmd.none
            )

        UpdateEntry id task ->
            let
                operation =
                    Entry.update (Entry.Description task)
            in
            ( { model | entries = List.map operation model.entries }
            , Cmd.none
            )

        Delete id ->
            ( { model | entries = List.filter (\t -> Entry.id t /= id) model.entries }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | entries = List.filter (not << Entry.completed) model.entries }
            , Cmd.none
            )

        Check id isCompleted ->
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

        CheckAll isCompleted ->
            let
                updateEntry t =
                    Entry.update (Entry.Completed isCompleted) t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
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
            , viewEntryList model.currentDate model.visibility model.editingId model.entries
            ]
        , infoFooter
        ]


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


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


viewEntryList : Date -> Visibility -> Maybe EntryId -> List Entry -> Html Msg
viewEntryList currentDate visibility editingId entries =
    let
        onCurrentDate =
            \todo -> Date.compare currentDate (Entry.date todo) == EQ

        currentEntries =
            List.filter onCurrentDate entries
    in
    div []
        [ lazy3 viewEntries visibility editingId currentEntries
        , lazy2 viewControls visibility currentEntries
        ]


viewEntries : Visibility -> Maybe EntryId -> List Entry -> Html Msg
viewEntries visibility editingId entries =
    let
        isVisible todo =
            case visibility of
                Completed ->
                    Entry.completed todo

                Active ->
                    not (Entry.completed todo)

                All ->
                    True

        visibleEntries =
            List.filter isVisible entries

        allCompleted =
            List.all Entry.completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"

            else
                "visible"

        editingEntry entry =
            case editingId of
                Just id ->
                    id == Entry.id entry

                Nothing ->
                    False
    in
    section
        [ class "main"
        , style "visibility" cssVisibility
        ]
        [ input
            [ class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            , checked allCompleted
            , onClick (CheckAll (not allCompleted))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] <|
            List.map (\e -> viewKeyedEntry (editingEntry e) e)
                visibleEntries
        ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Bool -> Entry -> ( String, Html Msg )
viewKeyedEntry editing entry =
    ( String.fromInt (Entry.id entry)
    , lazy2 viewEntry editing entry
    )


viewEntry : Bool -> Entry -> Html Msg
viewEntry editing entry =
    let
        entryId =
            Entry.id entry
    in
    li
        [ classList
            [ ( "completed", Entry.completed entry )
            , ( "editing", editing )
            ]
        ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked (Entry.completed entry)
                , onClick (Check entryId (not (Entry.completed entry)))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry entryId) ]
                [ text (Entry.description entry) ]
            , button
                [ class "destroy"
                , onClick (Delete entryId)
                ]
                []
            ]
        , input
            [ class "edit"
            , value (Entry.description entry)
            , name "title"
            , id ("todo-" ++ String.fromInt entryId)
            , onInput (UpdateEntry entryId)
            , onBlur FinishEdit
            , onEnter FinishEdit
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : Visibility -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter Entry.completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy viewControlsCount entriesLeft
        , lazy viewControlsFilters visibility
        , lazy viewControlsClear entriesCompleted
        ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt entriesLeft) ]
        , text (item_ ++ " left")
        ]


viewControlsFilters : Visibility -> Html Msg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" All visibility
        , text " "
        , visibilitySwap "#/active" Active visibility
        , text " "
        , visibilitySwap "#/completed" Completed visibility
        ]


visibilitySwap : String -> Visibility -> Visibility -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text (visibilityText visibility) ]
        ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]


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
