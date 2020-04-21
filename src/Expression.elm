module Expression exposing
  (
    Expression,
    fromNumber,
    pi, e,
    pos, neg,
    add, sub, mul, div, pow,
    sin, cos, tan, abs, sgn, rnd, ln, lg, sqrt,
    solve
  )

import Basics

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
  = Sin | Cos | Tan
  | Abs | Sgn | Rnd
  | Ln  | Lg  | Sqrt

type Expression
  = Number Float
  | Constant Constant
  | Unary Unary Expression
  | Binary Binary Expression Expression
  | Function Function Expression

cFloat : Constant -> Float
cFloat c =
  case c of
    PI -> Basics.pi
    E -> Basics.e

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
    Sin  -> Basics.sin
    Cos  -> Basics.cos
    Tan  -> Basics.tan
    Abs  -> Basics.abs
    Sgn  -> \x -> if x == 0 then 0 else if x < 0 then -1 else 1
    Rnd  -> round >> toFloat
    Ln   -> logBase Basics.e
    Lg   -> logBase 10
    Sqrt -> Basics.sqrt

fromNumber : Float -> Expression
fromNumber = Number

pi : Expression
pi = Constant PI

e : Expression
e = Constant E

pos : Expression -> Expression
pos = Unary Plus

neg : Expression -> Expression
neg = Unary Minus

add : Expression -> Expression -> Expression
add = Binary Add

sub : Expression -> Expression -> Expression
sub = Binary Sub

mul : Expression -> Expression -> Expression
mul = Binary Mul

div : Expression -> Expression -> Expression
div = Binary Div

pow : Expression -> Expression -> Expression
pow = Binary Pow

sin : Expression -> Expression
sin = Function Sin

cos : Expression -> Expression
cos = Function Cos

tan : Expression -> Expression
tan = Function Tan

abs : Expression -> Expression
abs = Function Abs

sgn : Expression -> Expression
sgn = Function Sgn

rnd : Expression -> Expression
rnd = Function Rnd

ln : Expression -> Expression
ln = Function Ln

lg : Expression -> Expression
lg = Function Lg

sqrt : Expression -> Expression
sqrt = Function Sqrt

solve : Expression -> Float
solve x =
  case x of
    Number n     -> n
    Constant c   -> cFloat c
    Unary u y    -> uFunc u (solve y)
    Binary b y z -> bFunc b (solve y) (solve z)
    Function f y -> fFunc f (solve y)
