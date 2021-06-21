import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.scalatest.prop.TableDrivenPropertyChecks.*
import com.compiler.lexer.*

class LexerTest extends AnyFlatSpec with Matchers {
  it should "return a Char from input of a single char string using the Char parser" in {
    val input = "A"
    val expected = LexerSuccess("", 'A')

    val result = Lexer.char.tokenize(input)

    result shouldBe expected
  }

  it should "return a Digit from input of a single char string from 0-9 using the Digit Parser" in {
    val digits = ('0' to '9').map(_.toString).zipWithIndex
    val input = Table(("str", "ex"), digits: _*)

    forAll(input) { (str, ex) =>
      val expected = LexerSuccess("", ex.toString.head)
      val result = Lexer.digit.tokenize(str)

      result shouldBe expected
    }
  }

  it should "return a List of Chars from input of a multi char string using the many(char) parser" in {
    val input = "abcd"
    val expected = LexerSuccess("", List('a', 'b', 'c', 'd'))

    val result = Lexer.many(Lexer.char).tokenize(input)

    result shouldBe expected
  }

  it should "return an empty List of Chars from input of a empty string using the many(char) parser" in {
    val input = ""
    val expected = LexerSuccess("", List())

    val result = Lexer.many(Lexer.char).tokenize(input)

    result shouldBe expected
  }

  it should "return an empty List of Digits from input of a multiple letter string using the many(char) parser" in {
    val input = "abcd"
    val expected = LexerSuccess("abcd", List())

    val result = Lexer.many(Lexer.digit).tokenize(input)

    result shouldBe expected
  }

  it should "return a List of Chars from input of a multi char string using the some(char) parser" in {
    val input = "abcd"
    val expected = LexerSuccess("", List('a', 'b', 'c', 'd'))

    val result = Lexer.some(Lexer.char).tokenize(input)

    result shouldBe expected
  }

  it should "return a List of Digits from input of a single digit and multiple char string using the some(digit) parser" in {
    val input = "0abcd"
    val expected = LexerSuccess("abcd", List('0'))

    val result = Lexer.some(Lexer.digit).tokenize(input)

    result shouldBe expected
  }

  it should "fail from input of an empty string using the some(digit) parser" in {
    val input = ""
    val expected = LexerFailure(LexerError.PrematureEOF)

    val result = Lexer.some(Lexer.digit).tokenize(input)

    result shouldBe expected
  }

//  val tokenTable = Table(
//    ("character", "expectedToken"),
//    ("(", OpenParens),
//    (")", CloseParens),
//    ("1", IntegerType(1)),
//    ("1.0", FloatType(1.0)),
//    ("+", Add),
//    ("-", Subtract),
//    ("*", Multiply),
//    ("/", Divide),
//    (".", Decimal),
//    ("nil", NilType),
//    ("or", Or),
//    ("and", And),
//    ("not", Not),
//    ("print", PrintFn),
//  )
//  "A Lexer" should "map all the characters correctly" in {
//    forAll (tokenTable) { (character: String, expectedToken: Token) =>
//      val resultToken = Lexer[Token].tokenize(character)
//      resultToken shouldBe List(expectedToken)
//    }
//  }
//
//  it should "parse integers, floats and simple operations correctly" in {
//    val test = "(+ 2 3.4)"
//    val expectedTokens = List(OpenParens, Add, IntegerType(2), FloatType(3.4), CloseParens)
//    val createdTokens = Lexer.tokenizeString(test)
//    createdTokens shouldBe expectedTokens
//  }
//
//  it should "allow for nested expressions" in {
//    val test2 = "(/ (+ 2 3.5) 2)"
//    val expectedTokens = List(
//      OpenParens, Divide, OpenParens, Add, IntegerType(2), FloatType(3.5), CloseParens, IntegerType(2), CloseParens)
//    val createdTokens = Lexer.tokenizeString(test2)
//    createdTokens shouldBe expectedTokens
//  }
}
