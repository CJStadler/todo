-- Component representing a list of entries.


module Component.EntryList exposing (InternalMsg, Model, Msg(..), init, update, view, visibilityText)

import Browser.Dom as Dom
import Date exposing (Date)
import Entry exposing (Entry)
import Helpers exposing (onEnter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Task



-- MODEL


type Model
    = Model Record


type alias Record =
    { visibility : Visibility
    , editingId : Maybe Entry.Id
    }


type Visibility
    = All
    | Active
    | Completed


init : String -> Model
init vizString =
    let
        viz =
            if visibilityToString Active == vizString then
                Active

            else if visibilityToString Completed == vizString then
                Completed

            else
                All
    in
    Model { visibility = viz, editingId = Nothing }


visibilityText : Model -> String
visibilityText (Model r) =
    visibilityToString r.visibility


visibilityToString : Visibility -> String
visibilityToString visibility =
    case visibility of
        All ->
            "All"

        Active ->
            "Active"

        Completed ->
            "Done"



-- UPDATE


type Msg ext
    = Internal InternalMsg -- Messages for this component.
    | External ext -- Messages to be handled externally.


type
    InternalMsg
    -- Changes to the UI state.
    = NoOp
    | EditingEntry Entry.Id
    | FinishEdit
    | ChangeVisibility Visibility


update : InternalMsg -> Model -> ( Model, Cmd InternalMsg )
update msg (Model r) =
    case msg of
        NoOp ->
            ( Model r, Cmd.none )

        EditingEntry id ->
            let
                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( Model { r | editingId = Maybe.Just id }
            , Task.attempt (\_ -> NoOp) focus
            )

        FinishEdit ->
            ( Model { r | editingId = Maybe.Nothing }
            , Cmd.none
            )

        ChangeVisibility viz ->
            ( Model { r | visibility = viz }
            , Cmd.none
            )



-- VIEW


type alias Config msg =
    { check : Entry.Id -> Bool -> msg
    , checkAll : Bool -> msg
    , updateEntry : Entry.Id -> String -> msg
    , delete : Entry.Id -> msg
    , deleteComplete : msg
    }


view : Config msg -> Model -> List Entry -> Html (Msg msg)
view config (Model r) entries =
    div []
        [ viewEntries config (Model r) entries
        , viewControls config.deleteComplete r.visibility entries
        ]


viewEntries : Config msg -> Model -> List Entry -> Html (Msg msg)
viewEntries config (Model r) entries =
    let
        isVisible todo =
            case r.visibility of
                Completed ->
                    Entry.completed todo

                Active ->
                    not (Entry.completed todo)

                All ->
                    True

        visibleEntries =
            List.filter isVisible entries
                |> List.sortWith compareEntries

        allCompleted =
            List.all Entry.completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"

            else
                "visible"

        editingEntry entry =
            case r.editingId of
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
            , onClick (External (config.checkAll (not allCompleted)))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , ul [ class "todo-list" ] <|
            List.map (\e -> viewEntry config (editingEntry e) e)
                visibleEntries
        ]


compareEntries : Entry -> Entry -> Order
compareEntries a b =
    let
        toInt e =
            case Entry.completed e of
                True ->
                    1

                False ->
                    0
    in
    compare (toInt a) (toInt b)



-- VIEW INDIVIDUAL ENTRIES


viewEntry : Config msg -> Bool -> Entry -> Html (Msg msg)
viewEntry config editing entry =
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
            [ class "view-entry" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked (Entry.completed entry)
                , onClick (External (config.check entryId (not (Entry.completed entry))))
                ]
                []
            , label
                [ onDoubleClick (Internal (EditingEntry entryId)) ]
                [ text (Entry.description entry) ]
            , button
                [ class "destroy"
                , onClick (External (config.delete entryId))
                ]
                []
            ]
        , input
            [ class "edit"
            , value (Entry.description entry)
            , name "title"
            , id ("todo-" ++ String.fromInt entryId)
            , onInput (External << config.updateEntry entryId)
            , onBlur (Internal FinishEdit)
            , onEnter (Internal FinishEdit)
            ]
            []
        ]



-- VIEW CONTROLS


viewControls : msg -> Visibility -> List Entry -> Html (Msg msg)
viewControls deleteComplete visibility entries =
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
        [ viewControlsCount entriesLeft
        , viewControlsFilters visibility
        , viewControlsClear deleteComplete entriesCompleted
        ]


viewControlsCount : Int -> Html (Msg msg)
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


viewControlsFilters : Visibility -> Html (Msg msg)
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" All visibility
        , text " "
        , visibilitySwap "#/active" Active visibility
        , text " "
        , visibilitySwap "#/completed" Completed visibility
        ]


visibilitySwap : String -> Visibility -> Visibility -> Html (Msg msg)
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (Internal (ChangeVisibility visibility)) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text (visibilityToString visibility) ]
        ]


viewControlsClear : msg -> Int -> Html (Msg msg)
viewControlsClear deleteComplete entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick (External deleteComplete)
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]
