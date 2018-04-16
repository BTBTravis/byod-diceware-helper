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
            ( { model | query = str }, Cmd.none )



--Scroll id ->
--( model, scroll id )
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port scroll : String -> Cmd msg



-- VIEW


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


view : Model -> Html Msg
view model =
    let
        diceSections =
            split 4 <| split 216 model.dlist

        --(diceSections) = List.Extra.takeWhile (\a -> a.id == 11111) model.dlist
    in
        div []
            --app info bar
            [ div [ class "info" ]
                [ h1 [ class "title" ] [ text "BYOD Diceware Helper" ]
                , ul [ class "steps" ]
                    [ li [ class "subtitle" ] [ text "Step 1. Roll dice" ]
                    , li [ class "subtitle" ] [ text "Step 2. Look up number via search" ]
                    , li [ class "subtitle" ] [ text "Step 3. Repeat" ]
                    , li []
                        [ a [ href "" ] [ text "* for more information on the diceware method and credit" ] ]
                    ]
                ]
            , div [ class "searchandresults" ]
                [ div [ class "search" ]
                    [ input [ id "search_input", class "input is-large is-info", type_ "text", placeholder "dice roll results", onInput HandleInput ] [] ]
                , div [ class "results" ]
                    [ h1 [ class "title" ] [ text model.query ] ]
                ]
            , div [ class "dicelist" ] []
            ]



--(List.map (\a -> viewDiceItem a model.name) col)


viewDiceItem : DiceItem -> String -> Html msg
viewDiceItem item searchTerm =
    let
        --  <function> : Int -> String
        nums =
            toString item.id
                -- <function> : String -> List String
                |> String.split ""
                -- <function> : List String -> List { i : number, part : String, searchPart : Maybe.Maybe String }
                |> List.indexedMap
                    (\i part ->
                        { i = i
                        , part = part
                        , searchPart =
                            --Debug.log "match"
                            (\a ->
                                case a of
                                    Nothing ->
                                        False

                                    Just head ->
                                        case head of
                                            Nothing ->
                                                False

                                            Just num ->
                                                num == part
                            )
                            <|
                                List.head <|
                                    (\a -> List.concatMap (\match -> match.submatches) a)
                                    --<| Debug.log "regex"
                                    <|
                                        (\a -> Regex.find All a searchTerm) <|
                                            Regex.regex <|
                                                (\a -> String.concat [ "^\\d{", a, "}(\\d)" ]) <|
                                                    (toString i)
                        }
                    )
                |> List.foldl
                    (\item carry ->
                        { part = item.part
                        , searchPart =
                            --(List.reverse carry
                            (List.head carry
                                --|> List.head
                                |> (\a ->
                                        case a of
                                            Nothing ->
                                                item.searchPart

                                            Just prev ->
                                                (if prev.searchPart then
                                                    item.searchPart
                                                 else
                                                    False
                                                )
                                   )
                            )
                        }
                            :: carry
                    )
                    []
                --|> (\ a -> let whole = a in
                --List.map(\b -> { b | part = "s"})
                --)
                |> List.reverse

        --|> Debug.log "nums"
    in
        div [ class "dice_item", id (toString item.id) ]
            [ p [ class "nums" ] (List.map (\a -> Html.span [ classList [ ( "highlight", a.searchPart ) ] ] [ text a.part ]) nums)
            , p [] [ text item.chars ]
            ]
