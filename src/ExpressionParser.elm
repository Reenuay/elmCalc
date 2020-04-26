module ExpressionParser exposing (run)

import Parser exposing (Parser, DeadEnd, (|.), (|=), float, keyword, symbol)
import Pratt exposing (Config, constant, infixLeft, infixRight, literal, prefix)
import Expression exposing (Expression)

parenthesizedExpression : Config Expression -> Parser Expression
parenthesizedExpression config =
  Parser.succeed identity
    |. symbol "("
    |= Pratt.subExpression 6 config
    |. symbol ")"

funcExpression : String -> (Expression -> Expression) -> Config Expression -> Parser Expression
funcExpression name func config =
  Parser.succeed func
    |. keyword name
    |= parenthesizedExpression config

operatorExpression : Parser Expression
operatorExpression =
  Pratt.expression
    {
      oneOf =
        [
          prefix 3 (symbol "+") Expression.pos,
          prefix 3 (symbol "-") Expression.neg,
          constant (keyword "pi") Expression.pi,
          constant (keyword "e") Expression.e,
          literal (Parser.map Expression.fromNumber float),
          funcExpression "ln" Expression.ln,
          funcExpression "lg" Expression.lg,
          funcExpression "sin" Expression.sin,
          funcExpression "cos" Expression.cos,
          funcExpression "tan" Expression.tan,
          funcExpression "abs" Expression.abs,
          funcExpression "sgn" Expression.sgn,
          funcExpression "rnd" Expression.rnd,
          funcExpression "sqrt" Expression.sqrt,
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

expression : Parser Expression
expression =
  Parser.succeed identity
    |= operatorExpression
    |. Parser.end

run : String -> Result (List DeadEnd) Expression
run = Parser.run expression
