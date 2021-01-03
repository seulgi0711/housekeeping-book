module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Bulma.Classes as B
import Bulma.Helpers as BHelpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Task
import Time
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias DateTuple =
    ( Time.Zone, Time.Posix )


type alias Expense =
    { title : String
    , amount : Int
    , date : String
    }


type alias Model =
    { title : String
    , amount : Int
    , list : List Expense
    , error : String
    , today : String
    , date : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" 0 [] "" "" Nothing, clearForm )



-- UPDATE


type Msg
    = HandleInputTitle String
    | HandleInputAmount String
    | HandleInputDate String
    | HandleKeyDownInput Int
    | HandleClickInsert
    | HandleEmptyTitle
    | HandleFormValidate (Result String Model)
    | InsertExpense
    | ClearForm String
    | SetToday DateTuple
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearForm today ->
            ( { model | title = "", amount = 0, error = "", today = today }, focusTitle )

        HandleInputTitle title ->
            ( { model | title = title }, Cmd.none )

        HandleInputAmount value ->
            case String.toInt value of
                Just amount ->
                    ( { model | amount = amount }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        HandleInputDate date ->
            ( { model | date = Just date }, Cmd.none )

        HandleKeyDownInput key ->
            case key of
                13 ->
                    update InsertExpense model

                _ ->
                    ( { model | error = "" }, Cmd.none )

        HandleClickInsert ->
            update InsertExpense model

        InsertExpense ->
            update (HandleFormValidate (validateForm model)) model

        HandleFormValidate result ->
            case result of
                Err error ->
                    ( { model | error = error }, Cmd.none )

                Ok _ ->
                    ( appendExpense model, clearForm )

        HandleEmptyTitle ->
            ( model, Cmd.none )

        SetToday dateTuple ->
            ( { model | today = dateTupleToString dateTuple }, Cmd.none )

        None ->
            ( model, Cmd.none )


validateForm : Model -> Result String Model
validateForm model =
    if String.isEmpty model.title then
        Err "타이틀이 비어있습니다. 타이틀을 입력해주세요."

    else if model.amount <= 0 then
        Err "가격은 0 이상이어야 합니다."

    else
        Ok model


appendExpense : Model -> Model
appendExpense model =
    { model | list = model.list ++ [ Expense model.title model.amount (getDate model) ] }


focusTitle : Cmd Msg
focusTitle =
    Dom.focus "form-title" |> Task.attempt (\_ -> None)


clearForm : Cmd Msg
clearForm =
    getToday |> Task.perform ClearForm


getToday : Task.Task Never String
getToday =
    Task.map2 Tuple.pair Time.here Time.now
        |> Task.map dateTupleToString



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        navBar =
            nav [ BHelpers.classList [ B.navbar, B.isLink ], attribute "role" "navigation", attribute "ariaLabel" "main navigation" ]
                [ div [ class B.navbarBrand ]
                    [ a [ class B.navbarItem, href "/" ]
                        [ span [ BHelpers.classList [ B.title, B.is3, B.hasTextLight ] ] [ text "📘 Housekeeping Book" ]
                        ]
                    ]
                , div [ class B.navbarEnd ]
                    [ div [ class B.navbarItem ]
                        [ div [ class B.buttons ]
                            [ a [ BHelpers.classList [ B.button, B.isInfo ] ] [ strong [] [ text "Sign up" ] ]
                            , a [ BHelpers.classList [ B.button, B.isLight ] ] [ strong [] [ text "Log in" ] ]
                            ]
                        ]
                    ]
                ]

        container children =
            div [ BHelpers.classList [ B.container, "is-max-widescreen" ] ]
                [ div [ BHelpers.classList [ B.notification ] ] children ]

        sectionView title subtitle children =
            section [ class B.section ]
                [ div [ BHelpers.classList [ B.container, "is-max-widescreen" ] ]
                    ([ h1 [ class B.title ] [ text title ]
                     , h2 [ class B.subtitle ] [ text subtitle ]
                     ]
                        ++ children
                    )
                ]

        formInput inputType labelStr attrs =
            let
                inputAttrs =
                    [ class B.input, type_ inputType ] ++ attrs
            in
            div [ class B.field ]
                [ label [ class B.label ] [ text labelStr ]
                , div [ class B.control ] [ input inputAttrs [] ]
                ]

        renderForm =
            let
                date =
                    Maybe.withDefault model.today model.date
            in
            div [ BHelpers.classList [ B.columns, B.isFlex, "is-align-items-flex-end" ] ]
                [ div [ class B.column ] [ formInput "text" "지출 항목" [ id "form-title", onInput HandleInputTitle, onKeyDown HandleKeyDownInput, value model.title ] ]
                , div [ class B.column ] [ formInput "number" "지출 금액" [ onInput HandleInputAmount, onKeyDown HandleKeyDownInput, Html.Attributes.min "0", value (String.fromInt model.amount) ] ]
                , div [ class B.column ] [ formInput "date" "날짜" [ onInput HandleInputDate, value date ] ]
                , div [ class B.column ] [ button [ BHelpers.classList [ B.button, B.isLink, B.isLight ], type_ "button", onClick HandleClickInsert ] [ text "입력" ] ]
                ]

        renderError =
            if String.isEmpty model.error then
                Html.text ""

            else
                article [ BHelpers.classList [ B.message, B.isDanger, B.my4 ] ] [ div [ class B.messageBody ] [ text model.error ] ]

        totalAmount =
            model.list
                |> List.map (\e -> e.amount)
                |> List.foldr (\a b -> a + b) 0
                |> numbToStringWithComma

        renderTotalAmount =
            div [ BHelpers.classList [ B.isFlex, "is-justify-content-flex-end" ] ]
                [ div [ BHelpers.classList [ B.hasTextWeightBold, B.mr6 ] ] [ text "총 합" ]
                , div [] [ p [ class B.control ] [ text totalAmount ] ]
                ]

        renderExpenseList =
            table [ BHelpers.classList [ B.table, B.isInfo, B.isLight, B.isFullwidth ] ]
                [ thead []
                    [ tr []
                        [ th [] [ text "항목" ]
                        , th [] [ text "금액" ]
                        , th [] [ text "날짜" ]
                        ]
                    ]
                , model.list
                    |> List.map renderExpense
                    |> tbody []
                ]

        renderExpense expense =
            tr []
                [ td [] [ text expense.title ]
                , td [] [ text <| numbToStringWithComma expense.amount ]
                , td [] [ text expense.date ]
                ]

        renderEmptyExepnseList =
            if List.isEmpty model.list then
                div
                    [ BHelpers.classList [ B.notification, B.isInfo, B.isLight, B.isFullwidth ] ]
                    [ text "지출 항목을 입력해 주세요." ]

            else
                Html.text ""
    in
    { title = "Initial Page"
    , body =
        [ navBar
        , sectionView
            "지출 목록"
            "지출 항목을 추가하거나 삭제 할 수 있습니다."
            [ renderForm
            , renderError
            , renderTotalAmount
            , renderExpenseList
            , renderEmptyExepnseList
            ]
        ]
    }


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


splitEveryN : Int -> String -> List String
splitEveryN n string =
    if String.length string < n then
        [ string ] |> List.filter (complement String.isEmpty)

    else
        let
            split =
                String.slice 0 n string

            rest =
                splitEveryN n (String.slice n (String.length string) string)
        in
        ([ split ] ++ rest) |> List.filter (complement String.isEmpty)


numbToStringWithComma : Int -> String
numbToStringWithComma num =
    "₩ " ++ (num |> String.fromInt |> String.reverse |> splitEveryN 3 |> String.join "," |> String.reverse)


complement : (a -> Bool) -> a -> Bool
complement fn a =
    not <| fn a


clearToday : Cmd Msg
clearToday =
    Task.perform SetToday (Task.map2 Tuple.pair Time.here Time.now)


dateTupleToString : DateTuple -> String
dateTupleToString ( zone, posixTime ) =
    let
        year =
            Time.toYear zone posixTime |> String.fromInt

        month =
            Time.toMonth zone posixTime |> monthToString

        day =
            Time.toDay zone posixTime
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    year ++ "-" ++ month ++ "-" ++ day


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


getDate : Model -> String
getDate model =
    case model.date of
        Just date ->
            date

        Nothing ->
            model.today
