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
    updateOutput
        { targetWeight = ""
        , barbellWeightIs15Kg = False
        , output = Err ""
        }



-- UPDATE


type Msg
    = EditTargetWeight String
    | ToggleBarbellWeight


update : Msg -> Model -> Model
update msg model =
    updateInput msg model
        |> updateOutput


updateInput : Msg -> Model -> Model
updateInput msg model =
    case msg of
        EditTargetWeight targetWeight ->
            { model | targetWeight = targetWeight }

        ToggleBarbellWeight ->
            { model | barbellWeightIs15Kg = model.barbellWeightIs15Kg |> not }


updateOutput : Model -> Model
updateOutput model =
    let
        barbellWeight =
            model.barbellWeightIs15Kg |> barbellWeightIs15KgToGrams
    in
    { model | output = calculateOutput barbellWeight model.targetWeight }



-- VIEW


view : Model -> Html.Styled.Html Msg
view model =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            , Css.fontSize (Css.vh 7)
            , Css.height (Css.pct 100)
            , Css.textAlign Css.center
            , Css.backgroundColor (Css.rgb 38 38 38)
            , Css.color (Css.rgb 255 255 255)
            ]
        ]
        (viewContent model :: viewHacks)


viewHacks : List (Html.Styled.Html Msg)
viewHacks =
    [ viewHackHeight
    , viewHackDeviceView
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


viewHackDeviceView : Html.Styled.Html Msg
viewHackDeviceView =
    Html.Styled.node "meta"
        [ Html.Styled.Attributes.name "viewport"
        , Html.Styled.Attributes.attribute "content" "width=device-width, initial-scale=1"
        ]
        []


viewContent : Model -> Html.Styled.Html Msg
viewContent model =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.columnReverse
            , Css.flexGrow (Css.num 1)
            , Css.maxWidth (Css.px 700)
            ]
        ]
        [ viewInput model.targetWeight model.barbellWeightIs15Kg
        , viewOutput model.barbellWeightIs15Kg model.output
        ]


viewInput : String -> Bool -> Html.Styled.Html Msg
viewInput targetWeight barbellWeightIs15Kg =
    Html.Styled.div []
        [ viewBarbellToggle barbellWeightIs15Kg
        , viewTargetWeightInput targetWeight
        ]


viewTargetWeightInput : String -> Html.Styled.Html Msg
viewTargetWeightInput targetWeight =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.displayFlex
            ]
        ]
        [ Html.Styled.input
            [ Html.Styled.Events.onInput EditTargetWeight
            , Html.Styled.Attributes.type_ "number"
            , Html.Styled.Attributes.min "0"
            , Html.Styled.Attributes.placeholder "enter weight"
            , Html.Styled.Attributes.value targetWeight
            , maxWeight |> gramsToKgs |> String.fromFloat |> Html.Styled.Attributes.max
            , Html.Styled.Attributes.css
                [ Css.flexGrow (Css.num 1)
                , Css.fontSize (Css.vh 7)
                , Css.textAlign Css.center
                ]
            ]
            []
        ]


viewBarbellToggle : Bool -> Html.Styled.Html Msg
viewBarbellToggle barbellWeightIs15Kg =
    Html.Styled.div
        [ Html.Styled.Events.onClick ToggleBarbellWeight
        , Html.Styled.Attributes.css
            [ Css.flexGrow (Css.num 1)
            , Css.backgroundColor
                (Css.rgb
                    213
                    213
                    213
                )
            , Css.color (Css.rgb 0 0 0)
            ]
        ]
        [ barbellWeightIs15Kg
            |> not
            |> barbellWeightIs15KgToString
            |> (\other -> "Click for " ++ other ++ "kg barbell")
            |> Html.Styled.text
        ]


viewOutput : Bool -> Output -> Html.Styled.Html Msg
viewOutput barbellWeightIs15Kg output =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.justifyContent Css.center
            , Css.flexDirection Css.row
            , Css.displayFlex
            , Css.overflow Css.auto
            ]
        ]
        [ viewOutputContent barbellWeightIs15Kg output
        ]


viewOutputContent : Bool -> Output -> Html.Styled.Html Msg
viewOutputContent barbellWeightIs15Kg output =
    case output of
        Ok validOutput ->
            case validOutput of
                PlateList plates ->
                    plates |> viewPlates barbellWeightIs15Kg

                Suggestions suggestions ->
                    suggestions |> viewSuggestions

        Err err ->
            err
                |> Html.Styled.text
                |> List.singleton
                |> Html.Styled.div []


viewPlates : Bool -> List Plate -> Html.Styled.Html Msg
viewPlates barbellWeightIs15Kg plates =
    let
        baseElement =
            viewBase barbellWeightIs15Kg

        platesElements =
            plates
                |> List.map viewPlate
    in
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.columnReverse
            , Css.flexGrow (Css.num 1)
            , Css.overflow Css.auto
            ]
        ]
        (baseElement :: platesElements)


viewBase : Bool -> Html.Styled.Html Msg
viewBase barbellWeightIs15Kg =
    let
        figureElement =
            barbellWeightIs15Kg
                |> barbellWeightIs15KgToString
                |> Html.Styled.text
                |> List.singleton
                |> Html.Styled.b []
    in
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            ]
        ]
        [ viewBaseFlare
        , figureElement
        , viewBaseFlare
        ]


viewBaseFlare : Html.Styled.Html Msg
viewBaseFlare =
    Html.Styled.text "???????"


viewPlate : Plate -> Html.Styled.Html Msg
viewPlate plate =
    let
        plateColor =
            getPlateColor plate

        plateTextColor =
            getPlateTextColor plate

        plateWidth =
            getPlateWidth plate
    in
    plate
        |> gramsToKgs
        |> String.fromFloat
        |> Html.Styled.text
        |> List.singleton
        |> Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.borderBottomStyle Css.solid
                , Css.backgroundColor plateColor
                , Css.color plateTextColor
                , Css.width plateWidth
                ]
            ]
        |> List.singleton
        |> Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                ]
            ]


getPlateColor : Plate -> Css.Color
getPlateColor plate =
    case plate of
        -- gray
        500 ->
            Css.rgb 255 255 255

        -- gray
        1250 ->
            Css.rgb 53 53 53

        -- gray
        2500 ->
            Css.rgb 100 100 100

        -- black
        5000 ->
            Css.rgb 0 0 0

        -- green
        10000 ->
            Css.rgb 51 130 2

        -- yellow
        15000 ->
            Css.rgb 220 182 0

        -- blue
        20000 ->
            Css.rgb 0 71 138

        -- red
        25000 ->
            Css.rgb 125 0 0

        -- pink
        _ ->
            Css.rgb 255 51 207


getPlateTextColor : Plate -> Css.Color
getPlateTextColor plate =
    if plate == 500 then
        -- dark
        Css.rgb 0 0 0

    else
        -- light
        Css.rgb 255 255 255


getPlateWidth : Plate -> Css.Pct
getPlateWidth plate =
    let
        isSmall =
            List.member plate [ 500, 1250 ]
    in
    if isSmall then
        Css.pct 50

    else
        Css.pct 100


viewSuggestions : ( Grams, Grams ) -> Html.Styled.Html Msg
viewSuggestions ( lower, higher ) =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            ]
        ]
        [ "????" |> Html.Styled.text
        , Html.Styled.br [] []
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
        >> Html.Styled.text



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


barbellWeightIs15KgToGrams : Bool -> Grams
barbellWeightIs15KgToGrams =
    barbellWeightIs15KgToKgs
        >> toFloat
        >> kgsToGrams


barbellWeightIs15KgToKgs : Bool -> Int
barbellWeightIs15KgToKgs bool =
    if bool then
        15

    else
        20


barbellWeightIs15KgToString : Bool -> String
barbellWeightIs15KgToString =
    barbellWeightIs15KgToKgs
        >> String.fromInt


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
