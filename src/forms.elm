module Main exposing (Model, Msg(..), init, main, passwordLengthValidation, passwordMatchValidation, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Char exposing (isUpper, isLower, isDigit)
import String exposing (toList)
import List exposing (isEmpty, foldr)

-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String 
    }


init : Model
init =
    Model "" "" "" "" 



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String

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


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewInput "text"  "Age" model.age Age
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html Msg
viewValidation model =
    div []
        [ passwordMatchValidation model.password model.passwordAgain
        , passwordLengthValidation model.password
        , isAgeNumberValidation model.age
        , passwordUppercaseValidation model.password
        ]


passwordMatchValidation : String -> String -> Html msg
passwordMatchValidation password passwordAgain =
    if password == passwordAgain then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]


passwordLengthValidation : String -> Html msg
passwordLengthValidation password =
    if String.length password > 8 then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text "Password is not greater than 8 characters in length" ]

isAgeNumberValidation : String -> Html msg
isAgeNumberValidation age =
    if  (Maybe.withDefault 0 (String.toInt age) > 0) then
        div [ style "color" "green" ] [ text "OK"]
    
    else
        div [style "color" "red"] [text "Age is not a number!"]

passwordUppercaseValidation : String -> Html msg
passwordUppercaseValidation password =
    if isEmpty [List.map isUpper (toList password) ] then
        div [ style "color" "green" ] [ text "OK"]
    
    else 
        div [ style "color" "red"] [text "Password must contain at least one uppercase character"]