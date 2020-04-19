module ExpressionParser exposing (run)

import Parser exposing (Parser, (|.), (|=), float, keyword, symbol)
import Pratt exposing (constant, infixLeft, infixRight, literal, prefix)
import Expression exposing (Expression)

parenthesizedExpression : Pratt.Config Expression -> Parser Expression
parenthesizedExpression config =
  Parser.succeed identity
    |. symbol "("
    |= Pratt.subExpression 6 config
    |. symbol ")"

expression : Parser Expression
expression =
  Pratt.expression
    {
      oneOf =
        [
          literal (Parser.map Expression.fromNumber float),
          constant (keyword "pi") Expression.pi,
          constant (keyword "e") Expression.e,
          prefix 3 (symbol "+") Expression.pos,
          prefix 3 (symbol "-") Expression.neg,
          prefix 5 (keyword "sin") Expression.sin,
          parenthesizedExpression
        ],
      andThenOneOf =
        [
          infixLeft 1 (symbol "+") Expression.add,
          infixLeft 1 (symbol "-") Expression.sub,
          infixLeft 2 (symbol "*") Expression.mul,
          infixLeft 2 (symbol "/") Expression.div,
          infixRight 4 (symbol "^") Expression.pow
        ],
      spaces = Parser.spaces
    }

run : String -> Result (List Parser.DeadEnd) Expression
run = Parser.run expression
