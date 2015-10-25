import String
import List
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, targetChecked, on)
import StartApp.Simple as StartApp
import SysT exposing (..)


main =
  StartApp.start { model = model, view = view, update = update }

type alias Model =
  { program : SysT.E
  , trace : List SysT.E
  , numeric : Bool
  }

model = Model (Ap double (S(S(Z)))) [] False

programs = [ ("double 2", Ap double <| num 2)
           , ("double 4", Ap double <| num 4)
           , ("plus 2 3", Ap (Ap plus (num 2)) (num 3))
           , ("plus 5 6", Ap (Ap plus (num 5)) (num 6))
           , ("triangle 3", Ap triangle <| num 3)
           , ("triangle 6", Ap triangle <| num 6)
           ]
double = Lam Nat "x" (Rec Z "u" "v" (S (S (Var "v"))) (Var "x"))
triangle = Lam Nat "x" (Rec Z "u" "v" (S (Ap (Ap plus (Var "u")) (Var "v"))) (Var "x"))
plus = Lam Nat "a" (Lam Nat "b" (Rec (Var "a") "_" "t" (S (Var "t")) (Var "b")))


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let show = SysT.showE model.numeric
      trace = String.join "\n" <| List.reverse <| List.map show model.trace
  in div []
      [ output [ outputStyle ]
        [ text "Code:"
        , code [ style [("margin-left", "1em")] ] [ text <| show model.program ]
        , pre [] [ text trace ]
        ]
      , h1' "System Z"
      , programList address
      , checkbox address model.numeric Numeric "Use numbers instead of S and Z"
      , button [ onClick address Reset ] [ text "Reset" ]
      , button [ onClick address Step ] [ text "Step" ]
      , button [ onClick address Step ] [ text "Run" ]
      ]

h1' t = h1 [] [text t]

clickOption address (name, code) = li [] [ button [ btnLink, onClick address (SetCode code) ] [ text name ] ]
programList address = ul [] <| List.map (clickOption address) programs

checkbox : Signal.Address Action -> Bool -> (Bool -> Action) -> String -> Html
checkbox address state tag name =
  label [labelStyle]
    [ input
      [ type' "checkbox"
      , checked state
      , on "change" targetChecked (tag >> Signal.message address)
      ]
      []
  , text name
  ]


textareaStyle =
  style
    [ ("display", "block")
    , ("width", "400px")
    , ("height", "200px")
    , ("resize", "none")
    ]

outputStyle =
  style
    [ ("float", "right")
    , ("width", "70%")
    , ("border", "1px solid #999")
    , ("margin", "0")
    , ("margin-right", "10px")
    , ("padding", "10px")
    ]

labelStyle =
  style
    [ ("display", "block")
    ]

btnLink =
  style
    [ ("border-color", "transparent")
    , ("background-color", "transparent")
    , ("box-shadow", "none")
    ]

    -- ]
    -- [ button [ onClick address Decrement ] [ text "-" ]
    -- , div [] [ text (toString model) ]
    -- , button [ onClick address Increment ] [ text "+" ]
    -- ]


type Action = Reset
            | Step
            | Run
            | Numeric Bool
            | SetCode E

update action model =
  case action of
    Reset -> { model | trace <- [ ] }
    Step  ->
      let current =  Maybe.withDefault model.program (List.head model.trace)
          next    = SysT.step current
      in { model | trace <- next :: model.trace }
    Run   -> model
    Numeric n -> { model | numeric <- n }
    SetCode e -> { model | program <- e, trace <- [] }
