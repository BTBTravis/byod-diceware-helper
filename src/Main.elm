module Main exposing (..)

import Regex exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dice exposing (..)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL
--type alias DiceItem =
  --{ id : Int
  --, chars : String
  --}

--dicelist = [
    --DiceItem 11111 "a"
    --, DiceItem 11112 "a&p"
    --, DiceItem 11113 "a's"
    --, DiceItem 11114 "aa"
    --, DiceItem 11115 "aaa"
    --, DiceItem 11116 "aaaa"
    --, DiceItem 11121 "aaron"
    --, DiceItem 11122 "ab"
    --, DiceItem 11123 "aba"
    --, DiceItem 11124 "ababa"
    --, DiceItem 11125 "aback"
    --, DiceItem 11126 "abase"
    --, DiceItem 11131 "abash"
    --, DiceItem 11132 "abate"
    --, DiceItem 11133 "abbas"
    --, DiceItem 11134 "abbe"
    --, DiceItem 11135 "abbey"
    --]

type alias Model =
  { name : String
  , password : String
  , dlist : List DiceItem
  }


model : Model
model =
  Model "" "" dicelist 



-- UPDATE


type Msg
    = SearchNum String
    | Password String

update : Msg -> Model -> Model
update msg model =
  case msg of
    SearchNum numstr ->
      { model | name = numstr }

    Password password ->
      { model | password = password }


-- VIEW

--ss="level">
            --<div class="level-item has-text-centered">
 
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





