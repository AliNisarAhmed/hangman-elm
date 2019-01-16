module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Word


type alias LetterObj =
    { letter : String
    , isShowing : Bool
    }



-- Helper Functions


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


mapLetterObjToWord : List LetterObj -> List String
mapLetterObjToWord letterObj =
    List.map (\obj -> obj.letter) letterObj


checkGuess : Model -> ( Model, Cmd Msg )
checkGuess model =
    let
        word =
            mapLetterObjToWord model.word
    in
    if
        List.member model.guess word
        -- if the user has guessed a letter correctly
        -- then we need to unveil that letter
    then
        -- unveil the letter, then pass on the updated model to checkForWin
        -- unveilLetter was provided to List.map as partially applied with model, await LetterObj from the map
        checkForWin { model | word = List.map (unveilLetter model) model.word, guess = "" }

    else
        checkForLoss { model | wrongLetters = List.append model.wrongLetters (List.singleton model.guess), guess = "", turns = model.turns - 1 }


unveilLetter : Model -> LetterObj -> LetterObj
unveilLetter model letterObj =
    if letterObj.letter == model.guess then
        { letterObj | isShowing = True }

    else
        letterObj


checkForWin : Model -> ( Model, Cmd Msg )
checkForWin model =
    let
        result =
            model.word
                |> List.map (\obj -> obj.isShowing)
                |> List.all (\x -> x == True)
    in
    case result of
        True ->
            ( { model | gameStatus = "You Won with " ++ String.fromInt model.turns ++ " turns remaining!!!" }
            , Cmd.none
            )

        False ->
            -- TODO:
            ( model
            , Cmd.none
            )


checkForLoss : Model -> ( Model, Cmd Msg )
checkForLoss model =
    if model.turns == 0 then
        ( { model | gameStatus = "You lost (LOL)" }
        , Cmd.none
        )

    else
        ( model
        , Cmd.none
        )



---- MODEL ----


type alias Model =
    { guess : String
    , word : List LetterObj
    , wrongLetters : List String
    , gameStatus : String
    , turns : Int
    }


initModel : Model
initModel =
    { guess = ""
    , word = mapToLetterObj Word.word
    , wrongLetters = []
    , gameStatus = "Take a Guess"
    , turns = 5
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | WordInput String
    | FormSubmit
    | ResetGame



-- | CheckForWin


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        WordInput letter ->
            ( { model | guess = letter }
            , Cmd.none
            )

        FormSubmit ->
            checkGuess model

        ResetGame ->
            ( initModel
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ h1 [ id "title" ] [ text "Hangman Game" ]
        , div [ class "msg" ] [ text model.gameStatus ]
        , div [ class "wordDisplay" ] [ text (displayWord model.word) ]
        , div [ class "wrongLetters" ] [ text (String.concat model.wrongLetters) ]
        , div [ class "guessesLeft" ]
            [ p [] [ text ("Guesses Left: " ++ String.fromInt model.turns) ] ]
        , Html.form [ onSubmit FormSubmit ]
            [ input [ type_ "text", name "guess", required True, onInput WordInput, value model.guess ] []
            ]
        , button [ class "btn", onClick ResetGame ] [ text "New Word" ]
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
