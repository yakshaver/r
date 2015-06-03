package com.ck.parser.r

import org.parboiled2._

sealed trait E

case class Com(v: String) extends E
case class Ident(i: String) extends E

case class Pow(l: E, r: E) extends E
case class IntRem(l: E, r: E) extends E
case class IntDiv(l: E, r: E) extends E
case class Mul(l: E, r: E) extends E
case class Div(l: E, r: E) extends E
case class Add(l: E, r: E) extends E
case class Sub(l: E, r: E) extends E

case class Integer(i: Int) extends E
case class Decimal(d: Double) extends E

object Null extends E
object Na extends E
object Inf extends E
object Nan extends E

object True extends E
object False extends E

/**
 * This Parsing Expression Grammar parses a subset of the R language.
 */
class R(val input: ParserInput) extends Parser {

  import CharPredicate.{Digit, Digit19, HexDigit}
  
  def InputLine = rule { Expression | CompoundExpression ~ EOI }

  // Compound Expressions: 10.4.4
  def CompoundExpression = rule { WhiteSpace ~ '{' ~ zeroOrMore(Expression ~ ';') ~ WhiteSpace ~ '}' ~ WhiteSpace }
  
  // Operators: 10.3.6
  def Expression: Rule1[E] = rule {
    Term ~ zeroOrMore(
      WhiteSpace ~ '+' ~ WhiteSpace ~ Term ~> Add
    | WhiteSpace ~ '-' ~ WhiteSpace ~ Term ~> Sub)
  }

  def Term: Rule1[E] = rule {
    Term2 ~ zeroOrMore(
      WhiteSpace ~ '*' ~ WhiteSpace ~ Term2 ~> Mul
    | WhiteSpace ~ '/' ~ WhiteSpace ~ Term2 ~> Div)
  }

 def Term2: Rule1[E] = rule {
    Factor ~ zeroOrMore(
      WhiteSpace ~ "%%" ~ WhiteSpace ~ Factor ~> IntRem
    | WhiteSpace ~ "%/%" ~ WhiteSpace ~ Factor ~> IntDiv
    | WhiteSpace ~ "^" ~ WhiteSpace ~ Factor ~> Pow)
  }

  def Factor = rule { Comment | Constants | Identifier | NumberRule | Parens }

  def Parens = rule { '(' ~ Expression ~ ')' }
  
  def NumberRule: Rule1[E] = rule { DecimalRule | IntegerRule }
    
  def IntegerRule: Rule1[E] = rule { WhiteSpace ~ capture(IntegerProd) ~ WhiteSpace ~> ((i: String) => Integer(i.toInt))}

  def DecimalRule: Rule1[E] = rule { WhiteSpace ~ capture(optional(IntegerProd) ~ Frac ~ optional(Exp)) ~ WhiteSpace ~> ((d: String) => Decimal(d.toDouble)) }
  
  def IntegerProd = rule { optional('-') ~ (Digit19 ~ Digits | Digit) }
  
  def Frac = rule { "." ~ Digits }
  
  def Exp = rule { ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits }
  
  def Digits = rule { oneOrMore(Digit) }
    
  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  
  // Identifiers: 10.3.2
  def Identifier: Rule1[E] = rule { WhiteSpace ~ capture(IdentifierFirstCharacter ~ zeroOrMore(IdentifierCharactersTail)) ~ WhiteSpace ~> Ident}
  
  def IdentifierFirstCharacter = rule { Alpha | '.' ~ IdentifierCharactersNonNumeric}
  
  def Alpha = rule { "a" - "f" | "A" - "Z" }
  
  def IdentifierCharactersTail = rule { "0" - "9" | IdentifierCharactersNonNumeric}

  def IdentifierCharactersNonNumeric = rule { Alpha | "_" | '.' }
  
  // Tokens: 10.3
  // Constants: 10.3.1
  def Constants = rule { SpecialConstants | BooleanConstants | HexConstants }
  
  def SpecialConstants = rule { NullConst | NaConst | InfConst | NanConst }

  def BooleanConstants = rule { TrueConst | FalseConst }
  
  def HexConstants = rule { HexIntegerConst }
  
  def NullConst = rule { WhiteSpace ~ "NULL" ~ WhiteSpace ~> (() => Null) }
    
  def NaConst = rule { WhiteSpace ~ "NA" ~ WhiteSpace ~> (() => Na) }
  
  def InfConst = rule { WhiteSpace ~ "Inf" ~ WhiteSpace ~> (() => Inf) }
  
  def NanConst = rule { WhiteSpace ~ "NaN" ~ WhiteSpace ~> (() => Nan) }
  
  def TrueConst = rule { WhiteSpace ~ "TRUE" ~ WhiteSpace ~> (() => True) }
  
  def FalseConst = rule { WhiteSpace ~ "FALSE" ~ WhiteSpace ~> (() => False) }
  
  def HexIntegerConst: Rule1[E] = rule { "0x" ~ optional(capture(zeroOrMore(HexDigit))) ~> ((hex: Option[String]) => Integer(if (hex.isDefined) java.lang.Integer.parseInt(hex.get, 16) else 0)) }
  
  // Comments: 10.2
  
  def Comment = rule { WhiteSpace ~ "#" ~ capture(zeroOrMore(noneOf("\n\r"))) ~> Com }

}

object R extends App {
  override def main(args: Array[String]) = {
    if (args.length > 0) R(args(0)) 
  }
  def apply(s: String) = new R(s).InputLine.run()
}
