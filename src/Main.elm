module Main exposing (main)

import Browser
import Element
import Element.Input
import Html
import Widget
import Widget.Material
import Widget.Material.Color
import Widget.Material.Typography



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- CONST


maxWeight : Grams
maxWeight =
    1000000


constPlates : List Plate
constPlates =
    [ 500
    , 1250
    , 2500
    , 5000
    , 10000
    , 15000
    , 20000
    , 25000
    ]



-- MODEL


type alias Model =
    { targetWeight : String
    , barbellWeightIs15Kg : Bool
    , output : Output
    }


type ValidOutput
    = PlateList (List Plate)
    | Suggestions ( Grams, Grams )


type alias Output =
    Result String ValidOutput


type alias Grams =
    Int


type alias Plate =
    Grams


init : Model
init =
    { targetWeight = ""
    , barbellWeightIs15Kg = True
    , output = Err ""
    }



-- UPDATE


type Msg
    = EditTargetWeight String
    | EditBarbellWeightIs15Kg Bool


update : Msg -> Model -> Model
update msg model =
    updateInput msg model
        |> updateOutput


updateInput : Msg -> Model -> Model
updateInput msg model =
    case msg of
        EditTargetWeight targetWeight ->
            { model | targetWeight = targetWeight }

        EditBarbellWeightIs15Kg barbellWeight ->
            { model | barbellWeightIs15Kg = barbellWeight }


updateOutput : Model -> Model
updateOutput model =
    let
        barbellWeight =
            model.barbellWeightIs15Kg |> boolToBarbellWeight
    in
    { model | output = calculateOutput barbellWeight model.targetWeight }



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        globalConfig =
            Widget.Material.Typography.subtitle1
                ++ Widget.Material.Color.textAndBackground Widget.Material.darkPalette.background
                ++ [ Element.padding 8 ]
    in
    Element.layout
        globalConfig
        (Element.column
            [ Element.spacing 30
            ]
            [ viewInput model.targetWeight model.barbellWeightIs15Kg
            , viewOutput model.output
            ]
        )


viewInput : String -> Bool -> Element.Element Msg
viewInput targetWeight barbellWeightIs15Kg =
    Element.wrappedRow [ Element.spacing 20 ]
        [ Widget.textInput (Widget.Material.textInput Widget.Material.darkPalette)
            { chips = []
            , text = targetWeight
            , placeholder = Just (Element.Input.placeholder [] (Element.text "Weight"))
            , onChange = EditTargetWeight
            , label = "weight in kilograms"
            }
        , viewBarbellToggle barbellWeightIs15Kg
        ]


viewBarbellToggle : Bool -> Element.Element Msg
viewBarbellToggle barbellWeightIs15Kg =
    Element.column
        []
        [ barbellWeightIs15Kg |> barbellWeightIs15KgToString |> Element.text
        , Widget.switch (Widget.Material.switch Widget.Material.darkPalette)
            { description = "Toggle bar size"
            , onPress = barbellWeightIs15Kg |> not |> EditBarbellWeightIs15Kg |> Just
            , active = barbellWeightIs15Kg
            }
        ]


viewOutput : Output -> Element.Element Msg
viewOutput output =
    case output of
        Ok validOutput ->
            case validOutput of
                PlateList plates ->
                    plates |> viewPlates

                Suggestions suggestions ->
                    suggestions |> viewSuggestions

        Err err ->
            err |> viewWrappedText


viewPlates : List Plate -> Element.Element Msg
viewPlates =
    List.map viewPlate
        >> List.intersperse (Element.text " | ")
        >> Element.wrappedRow [ Element.spacing 10 ]


viewPlate : Grams -> Element.Element Msg
viewPlate =
    gramsToKgs
        >> String.fromFloat
        >> Element.text
        >> Element.el []


viewSuggestions : ( Grams, Grams ) -> Element.Element Msg
viewSuggestions ( lower, higher ) =
    Element.column []
        [ "🙅" |> Element.text
        , "Suggestions" |> Element.text
        , lower |> viewSuggestion "Lower: "
        , higher |> viewSuggestion "Higher: "
        ]


viewSuggestion : String -> Grams -> Element.Element Msg
viewSuggestion label =
    gramsToKgs
        >> String.fromFloat
        >> (++) label
        >> viewWrappedText


viewWrappedText : String -> Element.Element Msg
viewWrappedText =
    Element.text
        >> List.singleton
        >> Element.paragraph []



-- APP


calculateOutput : Grams -> String -> Output
calculateOutput barbellWeight =
    validateNumber
        >> Result.andThen validateHalfKgAble
        >> Result.andThen (validateGtBarbell barbellWeight)
        >> Result.andThen validateLtMax
        >> Result.map (calculateValidOutput barbellWeight)


validateNumber : String -> Result String Grams
validateNumber raw =
    let
        maybeFloat =
            raw
                |> String.toFloat
                |> Maybe.map kgsToGrams
    in
    Result.fromMaybe "not a number" maybeFloat


validateHalfKgAble : Grams -> Result String Grams
validateHalfKgAble weight =
    let
        isHalfKiloAble =
            weight |> modBy 500 |> (==) 0
    in
    if isHalfKiloAble then
        weight |> Ok

    else
        "not an even half kilo" |> Err


validateGtBarbell : Grams -> Grams -> Result String Grams
validateGtBarbell barbellWeight weight =
    let
        isGtBarbell =
            weight |> (<) barbellWeight
    in
    if isGtBarbell then
        weight |> Ok

    else
        "weight needs to be greater than weight of barbell" |> Err


validateLtMax : Grams -> Result String Grams
validateLtMax weight =
    let
        isLtMax =
            weight |> (>) maxWeight
    in
    if isLtMax then
        weight |> Ok

    else
        maxWeight
            |> gramsToKgs
            |> String.fromFloat
            |> (++) "max weight allowed is "
            |> Err


calculateValidOutput : Grams -> Grams -> ValidOutput
calculateValidOutput barbellWeight weight =
    if getIsIncomplete barbellWeight weight then
        Suggestions (calcSuggestions barbellWeight weight)

    else
        PlateList (calcPlates barbellWeight weight)


calcSuggestions : Grams -> Grams -> ( Grams, Grams )
calcSuggestions barbellWeight weight =
    Tuple.pair
        (weight |> findNextCompleteWeight barbellWeight -500)
        (weight |> findNextCompleteWeight barbellWeight 500)


kgsToGrams : Float -> Grams
kgsToGrams =
    (*) 1000
        >> round


gramsToKgs : Grams -> Float
gramsToKgs =
    toFloat
        >> (*) 0.001


boolToBarbellWeight : Bool -> Grams
boolToBarbellWeight bool =
    if bool then
        15000

    else
        20000


barbellWeightIs15KgToString : Bool -> String
barbellWeightIs15KgToString bool =
    if bool then
        "15 kg barbell"

    else
        "20 kg barbell"


findNextCompleteWeight : Grams -> Grams -> Grams -> Grams
findNextCompleteWeight barbellWeight step weight =
    let
        nextWeight =
            weight + step
    in
    if getIsIncomplete barbellWeight nextWeight then
        findNextCompleteWeight barbellWeight step nextWeight

    else
        nextWeight


getIsIncomplete : Grams -> Grams -> Bool
getIsIncomplete barbellWeight weight =
    weight
        |> calcPlates barbellWeight
        |> List.sum
        |> (\sum -> weight - (sum * 2))
        |> (\x -> x - barbellWeight)
        |> (\remaining -> remaining > 0)


calcPlates : Grams -> Grams -> List Plate
calcPlates barbellWeight weight =
    let
        plateWeightPerSide =
            toFloat (weight - barbellWeight) / 2 |> round
    in
    calcPlatesPerSide plateWeightPerSide []


calcPlatesPerSide : Grams -> List Plate -> List Plate
calcPlatesPerSide remaining plates =
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
                calcPlatesPerSide (remaining - nextPlate) (plates ++ [ nextPlate ])


heaviestPlateNotExceeding : Grams -> Maybe Plate
heaviestPlateNotExceeding weight =
    constPlates
        |> List.filter ((>=) weight)
        |> List.maximum
