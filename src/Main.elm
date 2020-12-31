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


type alias Expense =
    { title : String
    , amount : Int
    }


type alias Model =
    { title : String
    , amount : Int
    , list : List Expense
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" 0 [] "", Cmd.none )



-- UPDATE


type Msg
    = HandleInputTitle String
    | HandleInputAmount String
    | HandleKeyDownInput Int
    | HandleClickInsert
    | HandleEmptyTitle
    | HandleFormValidate (Result String Model)
    | InsertExpense
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleInputTitle title ->
            ( { model | title = title }, Cmd.none )

        HandleInputAmount value ->
            case String.toInt value of
                Just amount ->
                    ( { model | amount = amount }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        HandleKeyDownInput key ->
            if key == 13 then
                update InsertExpense model

            else
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
                    ( appendAndClearForm model, focusTitle )

        HandleEmptyTitle ->
            ( model, Cmd.none )

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


appendAndClearForm : Model -> Model
appendAndClearForm model =
    model |> appendForms |> clearForms


appendForms : Model -> Model
appendForms model =
    { model | list = model.list ++ [ Expense model.title model.amount ] }


clearForms : Model -> Model
clearForms model =
    { model | title = "", amount = 0 }


focusTitle : Cmd Msg
focusTitle =
    Dom.focus "form-title" |> Task.attempt (\_ -> None)



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
            div [ BHelpers.classList [ B.columns, B.isFlex, "is-align-items-flex-end" ] ]
                [ div [ class B.column ] [ formInput "text" "지출 항목" [ id "form-title", onInput HandleInputTitle, onKeyDown HandleKeyDownInput, value model.title ] ]
                , div [ class B.column ] [ formInput "number" "지출 금액" [ onInput HandleInputAmount, onKeyDown HandleKeyDownInput, Html.Attributes.min "0", value (String.fromInt model.amount) ] ]
                , div [ class B.column ] [ button [ BHelpers.classList [ B.button, B.isLink, B.isLight ], type_ "button", onClick HandleClickInsert ] [ text "입력" ] ]
                ]

        renderError =
            if String.isEmpty model.error then
                Html.text ""

            else
                article [ BHelpers.classList [ B.message, B.isDanger, B.my4 ] ] [ div [ class B.messageBody ] [ text model.error ] ]

        renderTotalAmount =
            div [ BHelpers.classList [ B.isFlex, "is-justify-content-flex-end" ] ]
                [ div [ BHelpers.classList [ B.hasTextWeightBold, B.mr6 ] ] [ text "총 합" ]
                , div [] [ p [ class B.control ] [ text <| "₩ " ++ totalAmount ] ]
                ]

        totalAmount =
            model.list
                |> List.map (\e -> e.amount)
                |> List.foldr (\a b -> a + b) 0
                |> String.fromInt

        renderExpenseList =
            table [ BHelpers.classList [ B.table, B.isInfo, B.isLight, B.isFullwidth ] ]
                [ thead []
                    [ tr []
                        [ th [] [ text "지출 항목" ]
                        , th [] [ text "지출 금액" ]
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
