package com.compiler.lexer

import com.compiler.lexer.LexerError
import com.compiler.lexer.LexerFailure

import scala.annotation.tailrec
import scala.util.Try

trait Lexer[+A] { self =>
  def tokenize(text: String): LexerResult[A]

  def zip[B](that: Lexer[B]): Lexer[(A, B)] = zipWith(that)((_, _))

  def zipWith[B, C](that: Lexer[B])(f: (A, B) => C): Lexer[C] = { input =>
    self.tokenize(input) match {
      case LexerFailure(err) => LexerFailure(err)
      case LexerSuccess(cont, a) =>
        that.tokenize(cont).map(f(a, _))
    }
  }

  def or[B >: A](that: Lexer[B]): Lexer[B] = { input =>
    self.tokenize(input) match {
      case LexerFailure(error) => that.tokenize(input)
      case LexerSuccess(cont, value) => LexerSuccess(cont, value)
    }
  }

  def map[B](f: A => B): Lexer[B] = { text =>
    self.tokenize(text).map(f)
  }

  def emap[B](f: A => Either[LexerError, B]): Lexer[B] = { input =>
    self.tokenize(input) match {
      case LexerFailure(err) => LexerFailure(err)
      case LexerSuccess(cont, value) =>
        f(value).fold(
          LexerFailure.apply,
          LexerSuccess(cont, _)
        )
    }
  }

  def filter(pred: A => Boolean) = Lexer.filter(self)(pred)

  def flatMap[B](f: A => Lexer[B]): Lexer[B] = { input =>
    self.tokenize(input).flatMap(a => f(a).tokenize(input))
  }

  def cons[B >: A](that: Lexer[List[B]]): Lexer[List[B]] = self.zipWith(that)(_ :: _)

  def append[B >: A, C](that: Lexer[List[C]])(using ev: B =:= List[C]): Lexer[List[C]] =
    self.zipWith(that)((b, d) => ev(b) ++ d)

  def recoverWith[B >: A](handler: LexerError => B): Lexer[B] = { input =>
    tokenize(input) match {
      case LexerFailure(error) => LexerSuccess(input, handler(error))
      case LexerSuccess(cont, value) => LexerSuccess(cont, value)
    }
  }
}

object Lexer {
  def success[A](a: A): Lexer[A] = { input => LexerSuccess(input, a) }
  def failure(msg: String): Lexer[Nothing] = { _ => LexerFailure(LexerError.Custom(msg)) }

  val char: Lexer[Char] = { text =>
    text
      .headOption
      .fold(LexerFailure(LexerError.PrematureEOF))(LexerSuccess(text.tail, _))
  }

  def filter[A](lexer: Lexer[A])(p: A => Boolean): Lexer[A] =
    lexer
      .flatMap(a =>
        if (p(a)) success(a)
        else failure("Invalid")
      )

  def append[A](self: Lexer[List[A]], that: Lexer[List[A]]): Lexer[List[A]] =
    self.zip(that).map(_ ++ _)

  val digit: Lexer[Char] = filter(char)(_.isDigit)

  def many[A](l: Lexer[A]): Lexer[List[A]] = { text =>
    def inner(r: LexerSuccess[List[A]]): LexerResult[List[A]] =
      l.tokenize(r.cont).fold(_ => r) { (cont, a) =>
        inner(LexerSuccess(cont, a :: r.value))
      }

    inner(LexerSuccess(text, Nil)).map(_.reverse)
  }

  def maybe[A](lexer: Lexer[A]): Lexer[Option[A]] = lexer.map(Option.apply).recoverWith(_ => None)

  def some[A](l: Lexer[A]): Lexer[List[A]] = l cons many(l)

  def charOf(c: Char): Lexer[Char] = filter(char)(_ == c)

  val posOrNeg: Lexer[Char] = charOf('+') or charOf('-')

  val decimal: Lexer[Char] = charOf('.')

  val int: Lexer[Int] = (maybe(posOrNeg).map(_.toList) append many(digit)).map(_.mkString).map(_.toInt)

  val float: Lexer[Float] =
    maybe(posOrNeg)
      .map(_.toList)
      .append(many(digit))
      .append(
        maybe(decimal cons many(digit))
          .map(_.toList.flatten)
      )
      .map(_.mkString)
      .map(_.toFloat)
}
