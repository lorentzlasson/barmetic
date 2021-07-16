module Main exposing (main)

import Browser
import Element
import Element.Input
import Html



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- CONST


maxWeight : Int
maxWeight =
    1000000


weights : { barbell : number, plates : List Plate }
weights =
    { barbell = 20000
    , plates =
        [ 500
        , 1250
        , 2500
        , 5000
        , 10000
        , 15000
        , 20000
        , 25000
        ]
    }



-- MODEL


type alias Model =
    { input : Input
    }


type alias Input =
    Result String Grams


type alias Grams =
    Int


type alias Plate =
    Int


init : Model
init =
    { input = Err ""
    }



-- UPDATE


type alias Msg =
    String


update : Msg -> Model -> Model
update msg model =
    { model | input = parseEntry msg }


parseEntry : String -> Input
parseEntry =
    validateNumber
        >> Result.andThen validateHalfKgAble
        >> Result.andThen validateGtBarbell
        >> Result.andThen validateLtMax



-- VIEW


view : Model -> Html.Html Msg
view { input } =
    Element.layout []
        (Element.column [ Element.spacing 10 ]
            [ viewInput
            , viewResult input
            ]
        )


viewInput : Element.Element Msg
viewInput =
    Element.Input.text
        []
        { label = Element.Input.labelHidden "weight in kilograms"
        , onChange = identity
        , placeholder = Nothing
        , text = "" -- not storing raw state of field
        }


viewResult : Input -> Element.Element Msg
viewResult entry =
    case entry of
        Err err ->
            err |> Element.text

        Ok grams ->
            let
                plates =
                    grams |> suggestPlates

                isIncomplete =
                    getIsIncomplete grams
            in
            if isIncomplete then
                viewCompleteness grams

            else
                plates
                    |> List.map viewPlate
                    |> Element.column []


viewPlate : Int -> Element.Element Msg
viewPlate =
    gramsToKgs
        >> String.fromFloat
        >> Element.text
        >> Element.el []


viewCompleteness : Grams -> Element.Element Msg
viewCompleteness weight =
    Element.column []
        [ "Impossible" |> Element.text
        , weight |> findNextCompleteWeight -500 |> viewSuggestion "lower"
        , weight |> findNextCompleteWeight 500 |> viewSuggestion "higher"
        ]


viewSuggestion : String -> Grams -> Element.Element Msg
viewSuggestion direction =
    gramsToKgs
        >> String.fromFloat
        >> (++) ("Suggested " ++ direction ++ ": ")
        >> Element.text



-- APP


validateNumber : String -> Input
validateNumber raw =
    let
        maybeFloat =
            raw
                |> String.toFloat
                |> Maybe.map kgsToGrams
    in
    Result.fromMaybe "not a number" maybeFloat


validateHalfKgAble : Grams -> Input
validateHalfKgAble grams =
    let
        isHalfKiloAble =
            grams |> modBy 500 |> (==) 0
    in
    if isHalfKiloAble then
        grams |> Ok

    else
        "not an even half kilo" |> Err


validateGtBarbell : Grams -> Input
validateGtBarbell grams =
    let
        isGtBarbell =
            grams |> (<) weights.barbell
    in
    if isGtBarbell then
        grams |> Ok

    else
        "weight needs to be greater than weight of barbell" |> Err


validateLtMax : Grams -> Input
validateLtMax grams =
    let
        isLtMax =
            grams |> (>) maxWeight
    in
    if isLtMax then
        grams |> Ok

    else
        grams
            |> gramsToKgs
            |> String.fromFloat
            |> (++) "max weight allowed is "
            |> Err


kgsToGrams : Float -> Int
kgsToGrams =
    (*) 1000
        >> round


gramsToKgs : Int -> Float
gramsToKgs =
    toFloat
        >> (*) 0.001


findNextCompleteWeight : Int -> Grams -> Grams
findNextCompleteWeight step weight =
    let
        nextWeight =
            weight + step
    in
    if getIsIncomplete nextWeight then
        findNextCompleteWeight step nextWeight

    else
        nextWeight


getIsIncomplete : Grams -> Bool
getIsIncomplete weight =
    weight
        |> suggestPlates
        |> List.sum
        |> (\sum -> weight - (sum * 2))
        |> (\x -> x - weights.barbell)
        |> (\remaining -> remaining > 0)


suggestPlates : Grams -> List Plate
suggestPlates weight =
    let
        plateWeightPerSide =
            toFloat (weight - weights.barbell) / 2 |> round
    in
    suggestPlatesPerSide plateWeightPerSide []


suggestPlatesPerSide : Int -> List Plate -> List Plate
suggestPlatesPerSide remaining plates =
    if remaining <= 0 then
        plates

    else
        let
            maybeNextPlate =
                heaviestPlateNotExceeding remaining
        in
        case maybeNextPlate of
            Nothing ->
                plates

            Just nextPlate ->
                suggestPlatesPerSide (remaining - nextPlate) (plates ++ [ nextPlate ])


heaviestPlateNotExceeding : Int -> Maybe Plate
heaviestPlateNotExceeding weight =
    weights.plates
        |> List.filter ((>=) weight)
        |> List.maximum
