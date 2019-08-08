-- Testing .gitignore
module Main exposing (Model, Msg(..), init, main, passwordLengthValidation, passwordMatchValidation, update, view, viewInput, viewValidation)

import Browser
import Char exposing (isDigit, isLower, isUpper)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (member)
import String exposing (toList)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , submit : Bool
    }


init : Model
init =
    Model "" "" "" "" False



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        Age age ->
            { model | age = age }

        Submit submit ->
            { model | submit =  True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewInput "text" "Age" model.age Age
        , div [] [ button [ onClick (Submit True) ] [ text "Submit"] ]
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html Msg
viewValidation model =
    if model.submit == True then
        div []
            [ passwordMatchValidation model.password model.passwordAgain
            , passwordLengthValidation model.password
            , isAgeNumberValidation model.age
            , passwordUppercaseValidation model.password
            , passwordLowercaseValidation model.password
            , passwordDigitValidation model.password
            ]
    else
        div [] [ text ""]




passwordMatchValidation : String -> String -> Html msg
passwordMatchValidation password passwordAgain =
    if password == passwordAgain then
        renderValidation "green" "OK" 

    else
        renderValidation "red" "Passwords do not match!"


passwordLengthValidation : String -> Html msg
passwordLengthValidation password =
    if String.length password > 8 then
        renderValidation "green" "OK" 

    else
        renderValidation "red"  "Password is not greater than 8 characters in length"


isAgeNumberValidation : String -> Html msg
isAgeNumberValidation age =
    if Maybe.withDefault 0 (String.toInt age) > 0 then
        renderValidation "green" "OK" 

    else
        renderValidation "red" "Age is not a number!"


passwordUppercaseValidation : String -> Html msg
passwordUppercaseValidation password =
    if member True (List.map isUpper (toList password)) then
        renderValidation "green" "OK" 

    else
        renderValidation "red" "Password must contain at least one uppercase character" 


passwordLowercaseValidation : String -> Html msg
passwordLowercaseValidation password =
    if member True (List.map isLower (toList password)) then
        renderValidation "green" "OK" 

    else
        renderValidation "red" "Password must contain at least one lowercase character!"


passwordDigitValidation : String -> Html msg
passwordDigitValidation password =
    if member True (List.map isDigit (toList password)) then
        renderValidation "green" "OK" 

    else
        renderValidation "red" "Password must contain at least one number!"

renderValidation : String -> String -> Html msg
renderValidation textColor validationText =
    div [ style "color" textColor ] [ text validationText ]