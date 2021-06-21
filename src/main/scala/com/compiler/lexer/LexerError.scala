package com.compiler.lexer

enum LexerError extends Throwable {
  case PrematureEOF
  case ConversionError
  case Custom(msg: String)
}
