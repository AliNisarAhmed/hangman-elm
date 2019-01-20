module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Random
import Random.Extra
import Regex
import Task
import Word



{- each word is broken into a list of LetterObj e.g. "elm" => [{letter: "e", isShowing: False}, ...] -}


type alias LetterObj =
    { isShowing : Bool
    , letter : String
    }


type GameCondition
    = Win
    | Lose
    | Play



-- Helper Functions


mapToLetterObj : String -> List LetterObj
mapToLetterObj str =
    str
        |> String.split ""
        |> List.map (LetterObj False)


displayWord : List LetterObj -> List String
displayWord list =
    list
        |> List.map showLetter


showLetter : LetterObj -> String
showLetter obj =
    case obj.isShowing of
        True ->
            obj.letter

        False ->
            "*"


mapLetterObjToWord : List LetterObj -> List String
mapLetterObjToWord letterObj =
    List.map (\obj -> String.toLower obj.letter) letterObj


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
        -- unveilLetter was provided to List.map as partially applied with model, awaits letterObj from the map
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
            ( { model | gameStatus = "You Won with " ++ String.fromInt model.turns ++ " turns remaining!!!", gameCondition = Win }
            , callToActionCommand
            )

        False ->
            ( model
            , Cmd.none
            )


checkForLoss : Model -> ( Model, Cmd Msg )
checkForLoss model =
    if model.turns == 0 then
        ( { model | gameStatus = "You lost (LOL)", gameCondition = Lose }
        , Task.perform ChangeMsg (Process.sleep 1500)
        )

    else
        ( model
        , Cmd.none
        )


letterRegex : Regex.Regex
letterRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "^[a-zA-Z]{0,1}$"


compareIndex : List Int -> Int -> LetterObj -> LetterObj
compareIndex listOfRandomLetters index letterObj =
    let
        bool =
            List.member index listOfRandomLetters
    in
    case bool of
        True ->
            { letterObj | isShowing = True }

        False ->
            letterObj



---- MODEL ----


type alias Model =
    { guess : String
    , word : List LetterObj
    , wrongLetters : List String
    , gameStatus : String
    , gameCondition : GameCondition
    , turns : Int
    , callToAction : Bool
    }


getWordCommand : Cmd Msg
getWordCommand =
    Random.generate NewNumber (Random.int 0 (Array.length Word.wordList))


getRandomIntList : Int -> Cmd Msg
getRandomIntList length =
    Random.generate RevealRandomLetters (Random.list (length // 3) (Random.int 0 length))


callToActionCommand : Cmd Msg
callToActionCommand =
    Task.perform CallToAction (Process.sleep 1500)


initModel : Model
initModel =
    { guess = ""
    , wrongLetters = []
    , gameStatus = "Take a guess"
    , turns = 5
    , word = mapToLetterObj ""
    , gameCondition = Play
    , callToAction = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , getWordCommand
    )



---- UPDATE ----


type Msg
    = NoOp
    | WordInput String
    | FormSubmit
    | ResetGame
    | RevealWord ()
    | ChangeMsg ()
    | NewNumber Int
    | CallToAction ()
    | RevealRandomLetters (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        NewNumber num ->
            let
                modifiedWord =
                    mapToLetterObj <| Maybe.withDefault "secret" (Array.get num Word.wordList)
            in
            ( { model | word = modifiedWord }
            , getRandomIntList (List.length modifiedWord)
            )

        RevealRandomLetters list ->
            let
                modifiedWord =
                    List.indexedMap (compareIndex list) model.word
            in
            ( { model | word = modifiedWord }
            , Cmd.none
            )

        WordInput letter ->
            if Regex.contains letterRegex letter then
                ( { model | guess = String.toLower letter }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        FormSubmit ->
            checkGuess model

        ResetGame ->
            ( initModel
            , getWordCommand
            )

        CallToAction _ ->
            ( { model | callToAction = True }
            , Cmd.none
            )

        RevealWord _ ->
            ( { model | word = List.map (\lettObj -> { lettObj | isShowing = True }) model.word }
            , Task.perform CallToAction (Process.sleep 1500)
            )

        ChangeMsg _ ->
            ( { model | gameStatus = "The Correct word was..." }
            , Task.perform RevealWord (Process.sleep 1500)
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ h1 [ id "title" ] [ text "Hangman Game" ]
        , div [ class "msg", class (gold model.gameCondition) ] [ text model.gameStatus ]
        , div [ class "wordDisplay" ] (displayLetters (displayWord model.word))
        , div [ class "wrongLetters" ] (displayWrongLetters model.wrongLetters)
        , div [ class "guessesLeft" ]
            [ p [] [ text ("Guesses Left: " ++ String.fromInt model.turns) ] ]
        , Html.form [ onSubmit FormSubmit ]
            [ input [ type_ "text", name "guess", required True, onInput WordInput, value model.guess, disabled (disableInput model.gameCondition) ] []
            ]
        , button [ class "btn", class (callToAction model.callToAction), onClick ResetGame, maxlength 1 ] [ text "New Word" ]
        ]



-- class setters --


gold : GameCondition -> String
gold gc =
    case gc of
        Win ->
            "gold"

        _ ->
            ""


callToAction : Bool -> String
callToAction bool =
    case bool of
        True ->
            "callToAction"

        False ->
            ""



-- view helpers --


displayLetters : List String -> List (Html Msg)
displayLetters list =
    List.map (\letter -> span [ class "letter" ] [ text letter ]) list


displayWrongLetters : List String -> List (Html Msg)
displayWrongLetters list =
    [ h4 [ class "wrongLetters__heading" ] [ text "Wrong Letters: " ]
    , div [ class "wrongLetters__container" ]
        [ p [ class "wrongLetters__text" ]
            [ List.map (\letter -> letter ++ ", ") (List.take (List.length list - 1) list)
                ++ List.drop (List.length list - 1) list
                |> String.concat
                |> text
            ]
        ]
    ]


disableInput : GameCondition -> Bool
disableInput gc =
    case gc of
        Play ->
            False

        _ ->
            True



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
