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
 
view : Model -> Html Msg
view model =
  div []
    [ nav [ class "level" ] [div [ class "level-item has-text-centered" ]
        [div [ class "field"] [div [class "control"]
            [ input [ class "input is-large is-info", type_ "text", placeholder "Number", onInput SearchNum ] []
        ]]]]
    --, viewValidation model
    , div [] (List.map (\n -> viewDiceItem n model.name) model.dlist)
    ]


--viewValidation : Model -> Html msg
--viewValidation model =
  --let
    --(color, message) =
      --if model.password == model.password then
        --("green", "OK")
      --else
        --("red", "Passwords do not match!")
  --in
      --div [] 
          --[div [ style [("color", color)] ] [ text message ]
          --, div [] (List.map (\n -> viewDiceItem n model.name) model.dlist)
          --]

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
            --|> Debug.log "nums"
    in
        div [class "dice_item"] 
            [ p [] (List.map (\a -> span [ classList [ ("highlight", a.searchPart) ]] [text a.part]) nums)
            , p [] [text item.chars]
            ]





