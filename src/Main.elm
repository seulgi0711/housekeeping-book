module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
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
    { title = "Initial Page"
    , body =
        [ h1 []
            [ text "📘 Housekeeping Book"
            , small [ style "opacity" ".3", style "margin-left" ".5em" ] [ text "Save Money!!💵" ]
            ]
        , input [ id "form-title", type_ "text", onInput HandleInputTitle, onKeyDown HandleKeyDownInput, placeholder "지출 항목 입력", value model.title ] []
        , input [ type_ "number", onInput HandleInputAmount, onKeyDown HandleKeyDownInput, value (String.fromInt model.amount) ] []
        , button [ type_ "button", onClick HandleClickInsert ] [ text "입력" ]
        , div [] [ text model.error ]
        , div [] (renderLists model.list)
        ]
    }


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


renderLists : List Expense -> List (Html msg)
renderLists list =
    List.map renderExpense list


renderExpense : Expense -> Html msg
renderExpense expense =
    div []
        [ h4 [] [ text expense.title ]
        , div [] [ text (String.fromInt expense.amount) ]
        ]
