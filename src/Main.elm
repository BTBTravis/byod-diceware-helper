module Main exposing (..)

import Regex exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dice exposing (..)

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
    { name : String
    , password : String
    , dlist : List DiceItem
    }


type alias Flags = 
    { dicevals : List DiceItem } 

-- INIT
init : Flags -> (Model, Cmd Msg)
init flags =
  (Model "" "" flags.dicevals, Cmd.none)

-- UPDATE


type Msg
    = SearchNum String
    | Password String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SearchNum numstr ->
      ({ model | name = numstr }, Cmd.none)

    Password password ->
      ({ model | password = password }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW
split : Int -> List a -> List (List a)
split i list =
  case List.take i list of
    [] -> []
    listHead -> listHead :: split i (List.drop i list) 

view : Model -> Html Msg
view model =
    let
        (diceSections) = split 4 <|split 216 model.dlist 
        --(diceSections) = List.Extra.takeWhile (\a -> a.id == 11111) model.dlist
    in
  div []
    [ nav [ class "level" ] [div [ class "level-item has-text-centered" ]
        [div [ class "field"] [div [class "control"]
            [ input [ class "input is-large is-info", type_ "text", placeholder "Number", onInput SearchNum ] []
        ]]]]
    --, viewValidation model
    --, div [] (List.map (\n -> viewDiceItem n model.name) diceSections)
        --(List.map (\cols -> div [ class "columns" ] (List.map (\col -> div [ class "column" ] (viewDiceItem col model.name ) ) cols)) model.dlist)
    , div [ class "container dice_master" ] (List.map 
        (\cols -> div [ class "columns" ] 
            (List.map (\col -> div [ class "column" ] 
                (List.map (\a -> viewDiceItem a model.name) col)
            ) cols)
        ) diceSections)
    ]


viewDiceItem : DiceItem -> String -> Html msg
viewDiceItem item searchTerm = 
    let 
        --  <function> : Int -> String   
        nums = toString item.id 
            -- <function> : String -> List String
            |> String.split "" 
            -- <function> : List String -> List { i : number, part : String, searchPart : Maybe.Maybe String }
            |> List.indexedMap (\i part ->  
                {i = i
                , part = part
                , searchPart =
                    --Debug.log "match"  
                    (\a -> case a of
                        Nothing -> False
                        Just head -> case head of
                            Nothing -> False
                            Just num -> num == part
                    )
                    <| List.head
                    <| (\a -> List.concatMap (\match -> match.submatches) a) 
                    --<| Debug.log "regex"  
                    <| (\a -> Regex.find All a searchTerm)
                    <| Regex.regex 
                    <| (\a -> String.concat ["^\\d{", a, "}(\\d)"] )
                    <| (toString i)
                }
            )
            |> List.foldl (\item carry-> { part = item.part, searchPart = 
                --(List.reverse carry
                (List.head carry
                --|> List.head  
                |> (\a -> case a of
                        Nothing -> item.searchPart 
                        Just prev -> (if prev.searchPart then item.searchPart else False)
                    )
                )
            } :: carry) [] 
            --|> (\ a -> let whole = a in
                --List.map(\b -> { b | part = "s"}) 
               --)
            |> List.reverse
            |> Debug.log "nums"
    in
        div [class "dice_item"] 
            [ p [class "nums"] (List.map (\a -> Html.span [ classList [ ("highlight", a.searchPart) ]] [text a.part]) nums)
            , p [] [text item.chars]
            ]





