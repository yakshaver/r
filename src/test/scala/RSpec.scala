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
}