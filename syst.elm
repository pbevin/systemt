module SysT (E(..), T(..), step, showE, num) where

import Dict

type alias ID = String
type T = Nat | Arrow T T
type E = Var ID
       | Z
       | S E
       | Rec E ID ID E E
       | Lam T ID E
       | Ap E E

val e =
  case e of
    Z         -> True
    S e       -> val e
    Lam _ _ _ -> True
    _         -> False


num : Int -> E
num n =
  case n of
    0 -> Z
    _ -> S (num <| n-1)

step : E -> E
step e =
  case e of
    -- Var x     -> error ("Unbound variable " ++ x)
    Z         -> Z
    S e       -> S (step e)           -- 9.3a
    Ap (Lam t x e) e2 -> subst e2 x e -- 9.3d
    Ap e1 e2  ->
      if val e1
        then Ap e1 (step e2)          -- 9.3c
        else Ap (step e1) e2          -- 9.3b
    Rec e0 x y e1 e ->
      case e of
        Z -> e0  -- 9.3f
        S e' ->
          if val e
             then subst e' x (subst (Rec e0 x y e1 e') y e1) -- 9.3g
             else Rec e0 x y e1 (step e) -- 9.3e
        _ ->
          Rec e0 x y e1 (step e) -- 9.3e

subst : E -> ID -> E -> E
subst r z exp =
  case exp of
    Var x ->
      if x == z
         then r
         else Var x
    Lam t x body ->
      if x == z
         then Lam t x body
         else Lam t x (subst r z body)
    Ap e1 e2 ->
      Ap (subst r z e1) (subst r z e2)
    Rec e0 x y e1 e ->
      let e1' =
        if x == z || y == z
           then e1
           else subst r z e1
      in Rec (subst r z e0) x y e1' (subst r z e)
    S e -> S (subst r z e)
    Z -> Z

showE : Bool -> E -> String
showE numeric exp =
  case exp of
    Var x -> x
    Z -> if numeric then "0" else "Z"
    S e -> if numeric
              then showNum 1 e
              else "S " ++ showE numeric e
    Ap e1 e2 -> showE numeric e1 ++ "(" ++ showE numeric e2 ++ ")"
    Lam t x body -> "λ(" ++ x ++ " : " ++ toString t ++ ") { " ++ showE numeric body ++ " }"
    Rec e0 x y e1 e -> "rec(" ++ showE numeric e ++ ") { z -> " ++ showE numeric e0 ++ " | s(" ++ x ++ ") with " ++ y ++ " -> " ++ showE numeric e1 ++ " }"

showNum n e =
  case e of
    S e' -> showNum (n+1) e'
    Z -> toString n
    _ -> toString n ++ " + " ++ showE True e


