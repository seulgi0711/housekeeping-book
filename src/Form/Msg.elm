module Form.Msg exposing (..)

import Form.Model exposing (Model)


type Msg
    = HandleInputTitle String
    | HandleInputAmount String
    | HandleInputDate String
    | HandleKeyDownInput Int
    | HandleClickInsert
    | HandleEmptyTitle
    | HandleFormValidate (Result String Model)
    | InsertExpense
