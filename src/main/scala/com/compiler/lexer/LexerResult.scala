package com.compiler.lexer

sealed trait LexerResult[+A] { self =>
  def setCont(str: String): LexerResult[A] = self match {
    case LexerFailure(err) => LexerFailure(err)
    case LexerSuccess(_, value) => LexerSuccess(str, value)
  }

  def map[B](f: A => B): LexerResult[B] = self match {
    case LexerFailure(err)         => LexerFailure(err)
    case LexerSuccess(cont, value) => LexerSuccess(cont, f(value))
  }

  def flatMap[B](f: A => LexerResult[B]): LexerResult[B] = self match {
    case LexerFailure(err)         => LexerFailure(err)
    case LexerSuccess(cont, value) => f(value).setCont(cont)
  }

  def fold[B](failure: LexerError => B)(success: (String, A) => B): B = self match {
    case LexerFailure(err)      => failure(err)
    case LexerSuccess(cont, value) => success(cont, value)
  }
}

case class LexerSuccess[A](cont: String, value: A) extends LexerResult[A]
case class LexerFailure(error: LexerError) extends LexerResult[Nothing]
