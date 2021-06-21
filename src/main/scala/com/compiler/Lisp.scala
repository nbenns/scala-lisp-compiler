package com.compiler

import com.compiler.lexer.Lexer

object Lisp {
  enum Token {
    case IntegerType(value: Int)
    case FloatType(value: Float)
    case NilType
    case OpenParens
    case CloseParens
    case Add
    case Subtract
    case Multiply
    case Divide
    case Space
    case Decimal
    case And
    case Or
    case Not
    case VarName(value: String)
    case PrintFn
  }

  val int: Lexer[Token] = Lexer.int.map(Token.IntegerType.apply)
  val float: Lexer[Token] = Lexer.float.map(Token.FloatType.apply)

  val number = float or int

//  val grammer: Lexer[Token] = ???
}
