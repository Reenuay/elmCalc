module Expression exposing (Constant, Unary, Binary, Function, Expression)

type Constant
  = PI
  | E

type Unary
  = Plus
  | Minus

type Binary
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow

type Function
  = Abs
  | Sign
  | Round
  | Sqrt
  | Sin
  | Cos
  | Tan
  | Log
  | Log10

type Expression
  = Number Float
  | Constant Constant
  | Unary Unary Expression
  | Binary Binary Expression Expression
  | Function Function Expression
