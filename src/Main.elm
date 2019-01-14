module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Word


type alias LetterObj =
    { letter : String
    , isShowing : Bool
    }


mapToLetterObj : String -> List LetterObj
mapToLetterObj str =
    str
        |> String.split ""
        |> List.map (\x -> LetterObj x False)


displayWord : List LetterObj -> String
displayWord list =
    list
        |> List.map showLetter
        |> String.concat


showLetter : LetterObj -> String
showLetter obj =
    case obj.isShowing of
        True ->
            obj.letter

        False ->
            "*"



---- MODEL ----


type alias Model =
    { guess : String
    , word : List LetterObj
    }


init : ( Model, Cmd Msg )
init =
    ( { guess = ""
      , word = mapToLetterObj Word.word
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = WordInput String
    | FormSubmit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WordInput letter ->
            ( { model | guess = letter }
            , Cmd.none
            )

        FormSubmit ->
            ( { model | guess = "" }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ h1 [ id "title" ] [ text "Hangman Game" ]
        , div [ class "msg" ] [ text "Take a Guess" ]
        , div [] [ text (displayWord model.word) ]
        , div [] [ text "WrongLetters component here" ]
        , div [ class "guessesLeft" ]
            [ p [] [ text "Guesses Left: 5" ] ]
        , Html.form [ onSubmit FormSubmit ]
            [ input [ type_ "text", name "guess", required True, onInput WordInput ] []
            ]
        , button [ class "btn" ] [ text "New Word" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
