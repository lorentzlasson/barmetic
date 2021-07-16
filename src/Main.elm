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


type alias EnteredGrams =
    Int


type alias Entry =
    Result String EnteredGrams


type alias Plate =
    Int


type alias Model =
    { entry : Entry
    }


init : Model
init =
    { entry = Err ""
    }



-- UPDATE


type alias Msg =
    String


parseEntry : String -> Entry
parseEntry rawEntry =
    let
        maybeFloat =
            rawEntry |> String.toFloat
    in
    case maybeFloat of
        Nothing ->
            "not a number" |> Err

        Just float ->
            let
                grams =
                    float |> (*) 1000 |> round

                isHalfKiloAble =
                    grams |> modBy 500 |> (==) 0
            in
            if isHalfKiloAble then
                grams |> Ok

            else
                "not an even half kilo" |> Err


update : Msg -> Model -> Model
update msg model =
    { model | entry = parseEntry msg }



-- VIEW


view : Model -> Html.Html Msg
view { entry } =
    Element.layout []
        (Element.column [ Element.spacing 10 ]
            [ viewInput
            , viewResult entry
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


viewResult : Entry -> Element.Element Msg
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
    gramsToKg
        >> String.fromFloat
        >> Element.text
        >> Element.el []


viewCompleteness : EnteredGrams -> Element.Element Msg
viewCompleteness weight =
    Element.column []
        [ "Impossible" |> Element.text
        , weight |> findNextCompleteWeight -500 |> viewSuggestion "lower"
        , weight |> findNextCompleteWeight 500 |> viewSuggestion "higher"
        ]


viewSuggestion : String -> EnteredGrams -> Element.Element Msg
viewSuggestion direction =
    gramsToKg
        >> String.fromFloat
        >> (++) ("Suggested " ++ direction ++ ": ")
        >> Element.text



-- APP


gramsToKg : Int -> Float
gramsToKg =
    toFloat
        >> (*) 0.001


findNextCompleteWeight : Int -> EnteredGrams -> EnteredGrams
findNextCompleteWeight step weight =
    let
        nextWeight =
            weight + step
    in
    if getIsIncomplete nextWeight then
        findNextCompleteWeight step nextWeight

    else
        nextWeight


getIsIncomplete : EnteredGrams -> Bool
getIsIncomplete weight =
    weight
        |> suggestPlates
        |> List.sum
        |> (\sum -> weight - (sum * 2))
        |> (\x -> x - weights.barbell)
        |> (\remaining -> remaining > 0)


suggestPlates : EnteredGrams -> List Plate
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
