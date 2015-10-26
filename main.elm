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
  , done : Bool
  , stuck : Bool
  }

model = Model (Ap double (S(S(Z)))) [] False False False

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
        , if model.stuck then text "Stuck!" else text ""
        , if model.done then text "Done." else text ""
        ]
      , h1' "System Z"
      , p [] [ text "Sample programs:" ]
      , programList address
      , checkbox address model.numeric Numeric "Show numbers instead of S and Z"
      , button' address False Reset "Reset"
      , button' address model.done Step "Step"
      , button' address model.done Run "Run"
      ]

h1' t = h1 [] [text t]

button' address d action label =
  let attrs = [ onClick address action, disabled d ]
  in button attrs [ text label ]

clickOption address (name, code) = li [liStyle] [ button [ btnLink, onClick address (SetCode code) ] [ text name ] ]
programList address = ul [ulStyle] <| List.map (clickOption address) programs

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

ulStyle =
  style
    [ ("padding", "0")
    ]

liStyle =
  style
    [ ("list-style", "none")
    , ("margin", "0")
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
    Reset -> { model | trace <- [], done <- False, stuck <- False }
    Step ->
      let current =  Maybe.withDefault model.program (List.head model.trace)
      in addToTrace (SysT.step current) model

    Run ->
      if model.done
         then model
         else update Run (update Step model)

    Numeric n -> { model | numeric <- n }
    SetCode e -> update Reset { model | program <- e }

addToTrace next model =
  case next of
    SysT.Stuck ->  { model | done <- True, stuck <- True }
    SysT.Val e ->  { model | trace <- e :: model.trace, done <- True }
    SysT.Step e -> { model | trace <- e :: model.trace }
