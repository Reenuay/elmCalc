module Expression exposing
  (
    Constant(..),
    Unary(..),
    Binary(..),
    Function(..),
    Expression(..),
    solve
  )

type Constant
  = PI
  | E

type Unary
  = Plus
  | Minus

type Binary
  = Add | Sub
  | Mul | Div
  | Pow

type Function
  = Sin | Cos  | Tan
  | Abs | Sgn  | Rnd
  | Log | Sqrt | Log10

type Expression
  = Number Float
  | Constant Constant
  | Unary Unary Expression
  | Binary Binary Expression Expression
  | Function Function Expression

cFloat : Constant -> Float
cFloat c =
  case c of
    PI -> pi
    E -> e

uFunc : Unary -> Float -> Float
uFunc u =
  case u of
    Plus  -> identity
    Minus -> negate

bFunc : Binary -> Float -> Float -> Float
bFunc b =
  case b of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> (/)
    Pow -> (^)

fFunc : Function -> Float -> Float
fFunc f =
  case f of
    Sin   -> sin
    Cos   -> cos
    Tan   -> tan
    Abs   -> abs
    Sgn   -> \x -> if x == 0 then 0 else if x < 0 then -1 else 1
    Rnd   -> round >> toFloat
    Log   -> logBase e
    Sqrt  -> sqrt
    Log10 -> logBase 10

solve : Expression -> Float
solve x =
  case x of
    Number n     -> n
    Constant c   -> cFloat c
    Unary u y    -> uFunc u (solve y)
    Binary b y z -> bFunc b (solve y) (solve z)
    Function f y -> fFunc f (solve y)
