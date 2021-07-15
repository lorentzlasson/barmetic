module Main exposing (main)

import Browser
import Html
import Html.Events



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
    { barbell = 20
    , plates =
        [ 0.5
        , 1.25
        , 2.5
        , 5
        , 10
        , 15
        , 20
        , 25
        ]
    }



-- MODEL


type alias EnteredWeight =
    Int


type alias Plate =
    Float


type alias Model =
    { weight : EnteredWeight
    }


init : Model
init =
    { weight = 0
    }



-- UPDATE


type alias Msg =
    String


update : Msg -> Model -> Model
update msg model =
    let
        weight =
            msg
                |> String.toInt
                |> Maybe.withDefault 0
    in
    { model | weight = weight }



-- VIEW


view : Model -> Html.Html Msg
view { weight } =
    let
        plates =
            weight |> suggestPlates

        isIncomplete =
            getIsIncomplete weight
    in
    Html.div []
        [ Html.input [ Html.Events.onInput identity ] []
        , Html.br [] []
        , viewResult plates isIncomplete
        , viewCompleteness weight isIncomplete
        ]


viewPlate : Float -> Html.Html Msg
viewPlate =
    String.fromFloat
        >> Html.text
        >> List.singleton
        >> Html.li []


viewResult : List Float -> Bool -> Html.Html Msg
viewResult plates isIncomplete =
    if isIncomplete then
        Html.text ""

    else
        plates
            |> List.map viewPlate
            |> Html.ul []


viewSuggestion : String -> EnteredWeight -> Html.Html Msg
viewSuggestion direction suggestedWeight =
    suggestedWeight
        |> String.fromInt
        |> (++) ("Suggested " ++ direction ++ ": ")
        |> Html.text


viewCompleteness : EnteredWeight -> Bool -> Html.Html Msg
viewCompleteness weight isComplete =
    if isComplete then
        Html.div []
            [ "Impossible" |> Html.text
            , Html.br [] []
            , weight |> findNextCompleteWeight (\x -> x - 1) |> viewSuggestion "lower"
            , Html.br [] []
            , weight |> findNextCompleteWeight (\x -> x + 1) |> viewSuggestion "higher"
            ]

    else
        Html.text ""



-- APP


findNextCompleteWeight : (Int -> Int) -> EnteredWeight -> EnteredWeight
findNextCompleteWeight modify weight =
    let
        nextWeight =
            modify weight
    in
    if getIsIncomplete nextWeight then
        findNextCompleteWeight modify nextWeight

    else
        nextWeight


getIsIncomplete : EnteredWeight -> Bool
getIsIncomplete weight =
    weight
        |> suggestPlates
        |> List.sum
        |> (\sum -> toFloat weight - (sum * 2))
        |> (\x -> x - weights.barbell)
        |> (\remaining -> remaining > 0)


suggestPlates : EnteredWeight -> List Plate
suggestPlates weight =
    let
        plateWeightPerSide =
            toFloat (weight - weights.barbell) / 2
    in
    suggestPlatesPerSide plateWeightPerSide []


suggestPlatesPerSide : Float -> List Plate -> List Plate
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


heaviestPlateNotExceeding : Float -> Maybe Plate
heaviestPlateNotExceeding weight =
    weights.plates
        |> List.filter ((>=) weight)
        |> List.maximum
