--module Main exposing (..)


port module DiceHelper exposing (..)

import Regex exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias DiceItem =
    { id : Int
    , chars : String
    }


type alias Model =
    { query : String
    , dlist : List DiceItem
    }


type alias Flags =
    { dicevals : List DiceItem }


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


pw : List DiceItem -> String -> String
pw dlist query =
    String.split "" query
        |> split 5
        |> List.map String.concat
        |> List.map (getDiceItem dlist)
        |> List.foldr (\item str -> str ++ " " ++ item.chars) ""


getDiceItem : List DiceItem -> String -> DiceItem
getDiceItem dlist str =
    --  <function> : List [  Tuple // TODO: figure out how to comment this
    (List.map
        (\ditem ->
            ( (List.map2 (\a b -> a == b) (String.split "" (toString ditem.id)) (String.split "" str)
                |> List.filter (\bool -> bool)
                |> List.length
              )
            , ditem
            )
        )
        dlist
    )
        |> List.foldr
            (\a b ->
                -- double check the equality
                if (Tuple.first a) > (Tuple.first b) then
                    a
                else
                    b
            )
            ( 0, { id = 0, chars = "error" } )
        |> Tuple.second


padString : Int -> String -> String
padString i str =
    (i - (String.length str))
        |> (\total ->
                if total > 0 then
                    str ++ (String.repeat total "9")
                else
                    str
           )



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" flags.dicevals, Cmd.none )



-- UPDATE


type Msg
    = HandleInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleInput str ->
            ( { model | query = (onlyDiceVal str) }, Cmd.none )


onlyDiceVal : String -> String
onlyDiceVal str =
    String.split "" str
        |> List.filter (\char -> Regex.contains (regex "[1-6]") char)
        |> String.concat



--Scroll id ->
--( model, scroll id )
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port scroll : String -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        --app info bar
        [ div [ class "info" ]
            [ h1 [ class "title" ] [ text "BYOD Diceware Helper" ]
            , ul [ class "steps" ]
                [ li [ class "subtitle" ] [ text "Step 1. Roll dice" ]
                , li [ class "subtitle" ] [ text "Step 2. Enter number into input" ]
                , li [ class "subtitle" ] [ text "Step 3. Repeat" ]
                , li []
                    [ a [ href "http://world.std.com/~reinhold/diceware.html" ] [ text "* for more information on the diceware method and credit" ] ]
                ]
            ]
        , div [ class "searchandresults" ]
            [ div [ class "search" ]
                [ input [ id "search_input", class "input is-large is-info", type_ "text", placeholder "dice roll results", onInput HandleInput ] [] ]
            , div [ class "results" ]
                [ h1 [ class "title" ] [ text (pw model.dlist model.query) ] ]
            ]
        , div [ class "dicelist" ] [ diceList model.dlist ]
        ]


diceList : List DiceItem -> Html msg
diceList dlist =
    (List.map
        (\item ->
            div [ class "dice_item" ]
                [ p [ class "nums" ] [ text (toString item.id) ]
                , p [] [ text item.chars ]
                ]
        )
        dlist
    )
        |> ul []
