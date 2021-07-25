module Main exposing (main)

import Browser
import Css
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> Html.Styled.toUnstyled
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
    , barbellWeightIs15Kg = False
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


view : Model -> Html.Styled.Html Msg
view model =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            , Css.height (Css.vh 100)
            ]
        ]
        [ viewHackHeight
        , viewOutput model.output
        , viewInput model.targetWeight model.barbellWeightIs15Kg
        ]


viewHackHeight : Html.Styled.Html Msg
viewHackHeight =
    let
        hackCss =
            """
            html, body { height: 100% }
            """
    in
    Html.Styled.node "style"
        []
        [ Html.Styled.text hackCss
        ]


viewInput : String -> Bool -> Html.Styled.Html Msg
viewInput targetWeight barbellWeightIs15Kg =
    Html.Styled.div []
        [ viewBarbellToggle barbellWeightIs15Kg
        , Html.Styled.input
            [ Html.Styled.Events.onInput EditTargetWeight
            , Html.Styled.Attributes.type_ "number"
            , Html.Styled.Attributes.min "0"
            , maxWeight |> gramsToKgs |> String.fromFloat |> Html.Styled.Attributes.max
            , Html.Styled.Attributes.css [ Css.flexGrow (Css.num 1) ]
            ]
            []
        ]


viewBarbellToggle : Bool -> Html.Styled.Html Msg
viewBarbellToggle barbellWeightIs15Kg =
    Html.Styled.div []
        [ barbellWeightIs15Kg |> barbellWeightIs15KgToString |> Html.Styled.text
        , Html.Styled.input
            [ Html.Styled.Events.onCheck EditBarbellWeightIs15Kg
            , Html.Styled.Attributes.type_ "checkbox"
            ]
            []
        ]


viewOutput : Output -> Html.Styled.Html Msg
viewOutput output =
    case output of
        Ok validOutput ->
            case validOutput of
                PlateList plates ->
                    plates |> viewPlates

                Suggestions suggestions ->
                    suggestions |> viewSuggestions

        Err err ->
            err |> Html.Styled.text |> List.singleton |> Html.Styled.div []


viewPlates : List Plate -> Html.Styled.Html Msg
viewPlates =
    List.map viewPlate
        >> List.intersperse (Html.Styled.text " | ")
        >> Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.displayFlex
                ]
            ]


viewPlate : Grams -> Html.Styled.Html Msg
viewPlate =
    gramsToKgs
        >> String.fromFloat
        >> Html.Styled.text
        >> List.singleton
        >> Html.Styled.div []


viewSuggestions : ( Grams, Grams ) -> Html.Styled.Html Msg
viewSuggestions ( lower, higher ) =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            ]
        ]
        [ "🙅" |> Html.Styled.text
        , "Suggestions" |> Html.Styled.text
        , Html.Styled.br [] []
        , lower |> viewSuggestion "Lower: "
        , Html.Styled.br [] []
        , higher |> viewSuggestion "Higher: "
        ]


viewSuggestion : String -> Grams -> Html.Styled.Html Msg
viewSuggestion label =
    gramsToKgs
        >> String.fromFloat
        >> (++) label
        >> viewWrappedText


viewWrappedText : String -> Html.Styled.Html Msg
viewWrappedText =
    -- TODO: not wrapping
    Html.Styled.text



-- APP


calculateOutput : Grams -> String -> Output
calculateOutput barbellWeight =
    validateSomething
        >> Result.andThen validateNumber
        >> Result.andThen validateHalfKgAble
        >> Result.andThen (validateGtBarbell barbellWeight)
        >> Result.andThen validateLtMax
        >> Result.map (calculateValidOutput barbellWeight)


validateSomething : String -> Result String String
validateSomething raw =
    if String.isEmpty raw |> not then
        raw |> Ok

    else
        "no weight entered" |> Err


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
