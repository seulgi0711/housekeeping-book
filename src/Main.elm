module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" 0 [], Cmd.none )



-- UPDATE


type Msg
    = HandleInputTitle String
    | HandleInputAmount String
    | HandleKeyDownInput Int
    | HandleClickInsert
    | None


update : Msg -> Model -> ( Model, Cmd msg )
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
                ( appendAndClearForm model, Cmd.none )

            else
                ( model, Cmd.none )

        HandleClickInsert ->
            ( appendAndClearForm model, Cmd.none )

        None ->
            ( model, Cmd.none )


appendAndClearForm : Model -> Model
appendAndClearForm model =
    model |> appendForms |> clearForms


appendForms : Model -> Model
appendForms model =
    { model | list = model.list ++ [ Expense model.title model.amount ] }


clearForms : Model -> Model
clearForms model =
    { model | title = "", amount = 0 }



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
        , input [ type_ "text", onInput HandleInputTitle, placeholder "지출 항목 입력", value model.title ] []
        , input [ type_ "number", onInput HandleInputAmount, onKeyDown HandleKeyDownInput, value (String.fromInt model.amount) ] []
        , button [ type_ "button", onClick HandleClickInsert ] [ text "입력" ]
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
