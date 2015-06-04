package com.ck.parser.r

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import scala.util.Success
import scala.util.Failure

class RSpec extends FreeSpec with Matchers {

  "a mathematical" - {
    "addition expression should parse" in {
      R(" 1 + 1 ") should be(Success(Add(Integer(1), Integer(1))))
    }
    "subtraction expression should parse" in {
      R(" 1 - 1 ") should be(Success(Sub(Integer(1), Integer(1))))
    }
    "multiplication expression should parse" in {
      R(" 1 * 1 ") should be(Success(Mul(Integer(1), Integer(1))))
    }
    "division expression should parse" in {
      R(" 1 / 1 ") should be(Success(Div(Integer(1), Integer(1))))
    }
    "expression should parse with the correct precedence:" - {
      "addition and division" in {
        R(" 1 + 1 / 1 ") should be(Success(Add(Integer(1), Div(Integer(1), Integer(1)))))
        R(" 1 / 1 + 1") should be(Success(Add(Div(Integer(1), Integer(1)), Integer(1))))
      }
      "addition and multiplication" in {
        R(" 1 + 1 * 1 ") should be(Success(Add(Integer(1), Mul(Integer(1), Integer(1)))))
        R(" 1 * 1 + 1") should be(Success(Add(Mul(Integer(1), Integer(1)), Integer(1))))
      }
      "subtraction and division" in {
        R(" 1 - 1 / 1 ") should be(Success(Sub(Integer(1), Div(Integer(1), Integer(1)))))
        R(" 1 / 1 - 1") should be(Success(Sub(Div(Integer(1), Integer(1)), Integer(1))))
      }
      "subtraction and multiplication" in {
        R(" 1 - 1 * 1 ") should be(Success(Sub(Integer(1), Mul(Integer(1), Integer(1)))))
        R(" 1 * 1 - 1 ") should be(Success(Sub(Mul(Integer(1), Integer(1)), Integer(1))))
      }
    }
  }

  "an identifier" - {
    "which is valid should parse" in {
      R(" a ") should be(Success(Ident("a")))
      R(" .a ") should be(Success(Ident(".a")))
      R(" a_.1 ") should be(Success(Ident("a_.1")))
    }
    "which is invalid should fail to parse" in {
      R(" _a ") shouldBe a[Failure[_]]
    }
    "starting with a period followed by a number should be parsed as a decimal" in {
      R(" .1 ") should be(Success(Decimal(0.1)))
    }
  }

  "a compound expression" - {
    "that is valid should parse" in {
      R(" { 1 ; } ") should be(Success(Vector(Integer(1))))
      R(" { 1 ; 2 ; } ") should be(Success(Vector(Integer(1), Integer(2))))
      R(""" { 1 ; 
        2 ; } """) should be(Success(Vector(Integer(1), Integer(2))))
    }
  }

  "a constant expression" - {
    "if a valid constant should parse" in {
      R(" NULL ") should be(Success(Null))
      R(" NA ") should be(Success(Na))
      R(" Inf ") should be(Success(Inf))
      R(" NaN ") should be(Success(Nan))
      R(" TRUE ") should be(Success(True))
      R(" FALSE ") should be(Success(False))
    }
  }

  "a comment" - {
    "if valid should parse" in {
      R(" #adf aa ") should be(Success(Com("adf aa ")))
      R(""" #adf 
aa """) should be(Success(Com("adf ")))
    }
  }

  "a decimal" - {
    "if valid should parse" in {
      R(" .1 ") should be(Success(Decimal(0.1)))
      R(" 1.0 ") should be(Success(Decimal(1.0)))
      R(" 1.0e3 ") should be(Success(Decimal(1000.0)))
      R(" 1.0e+3 ") should be(Success(Decimal(1000.0)))
      R(" -1.0 ") should be(Success(Decimal(-1.0)))
      R(" -1.0e3 ") should be(Success(Decimal(-1000.0)))
      R(" -1.0e+3 ") should be(Success(Decimal(-1000.0)))
      R(" -1.0e-3 ") should be(Success(Decimal(-0.001)))
    }
  }

  "an integer" - {
    "if valid should parse" in {
      R(" 1 ") should be(Success(Integer(1)))
    }
  }

  "a hexidecimal constant" - {
    "if valid should parse" in {
      R("0xDEAF") should be(Success(Integer(java.lang.Integer.parseInt("DEAF", 16))))
    }
  }

  "a string constant" - {
    "if valid should parse" in {
      R(" 'a' ") should be(Success(Str("a")))
      R(""" "a" """) should be(Success(Str("a")))
      R(" '\n' ") should be(Success(Str("\n")))
      R(" '\000' ") should be(Success(Str("\000")))
      R(""" '\u0000' """) should be(Success(Str("\u0000")))
    }
  }

  "an assignment operator" - {
    "if valid should parse" in {
      R(" a <- 1 ") should be(Success(Ass(Ident("a"), Integer(1))))
      R(" a <- 1 + 2 ") should be(Success(Ass(Ident("a"), Add(Integer(1), Integer(2)))))
    }
  }

  "a function call" - {
    "if valid, should parse" in {
      R(" f ( ) ") should be(Success(FnApp(Ident("f"), Nil)))
      R(" f ( 1 ) ") should be(Success(FnApp(Ident("f"), Vector(Integer(1)))))
      R(" f ( 1 , 2 ) ") should be(Success(FnApp(Ident("f"), Vector(Integer(1), Integer(2)))))
    }
  }

  "a function definition" - {
    "if valid, should parse" in {
      R(" function ( ) { } ") should be(Success(Fn(Nil, Nil)))
      R(" function ( a ) { } ") should be(Success(Fn(Vector(Ident("a")), Nil)))
      R(" function( a , b ) { } ") should be(Success(Fn(Vector(Ident("a"), Ident("b")), Nil)))
      R("function() { 1 + 2 ; a <- 3 ; }") should be(Success(Fn(Nil, Vector(Add(Integer(1), Integer(2)), Ass(Ident("a"), Integer(3))))))
    }
  }

  "a comparison operator" - {
    "if valid, should parse" in {
      R(" 1 < 2 ") should be(Success(LessThan(Integer(1),Integer(2))))
      R(" 1 > 2 ") should be(Success(GreaterThan(Integer(1),Integer(2))))
      R(" 1 <= 2 ") should be(Success(LessThanOrEq(Integer(1),Integer(2))))
      R(" 1 >= 2 ") should be(Success(GreaterThanOrEq(Integer(1),Integer(2))))
      R(" 1 == 2 ") should be(Success(Eq(Integer(1),Integer(2))))
      R(" 1 != 2 ") should be(Success(NotEq(Integer(1),Integer(2))))
    }
  }

}