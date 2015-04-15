package com.alexknvl.libconfig.parser
import com.alexknvl.libconfig.ast._

import org.parboiled2._

import scala.util.{ Success, Failure }
import scala.util.control.NonFatal
import scala.annotation.switch

import java.lang.StringBuilder

private object CommonPredicates {
  val Digit = CharPredicate.Digit
  val Digit19 = CharPredicate.Digit19
  val HexDigit = CharPredicate.HexDigit
  val Alpha = CharPredicate.Alpha
  val AlphaNum = CharPredicate.AlphaNum

  val IdAlpha = AlphaNum ++ "_"
  val IdAlphaNum = IdAlpha ++ Digit
  val EOLChar = CharPredicate("\n\r")
  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"
  val PlusMinus = CharPredicate("+-")
}

class ConfigParser(val input: ParserInput) extends Parser with StringBuilding {
  import CommonPredicates._

  def Root: Rule1[ConfigGroup] = rule { WhiteSpace ~ Group0 ~ EOI }

  def Value: Rule1[ConfigValue] = rule {
    Number | Group | List | Array | True | False | String
  }

  def Setting = rule { Identifier ~ (ws(':') | ws('=')) ~ Value ~> {(_, _)} ~ optional(";") ~ WhiteSpace }
  def Group0: Rule1[ConfigGroup] = rule {
    zeroOrMore(Setting) ~> {items: Seq[(String, ConfigValue)] => ConfigGroup(Map(items: _*))}
  }
  def Group: Rule1[ConfigGroup] = rule {
    ws('{') ~ zeroOrMore(Setting) ~ ws('}') ~>
      {items: Seq[(String, ConfigValue)] => ConfigGroup(Map(items: _*))}
  }
  def Array = rule { ws('[') ~ zeroOrMore(Value).separatedBy(ws(',')) ~ ws(']') ~> (ConfigArray(_)) }
  def List = rule { ws('(') ~ zeroOrMore(Value).separatedBy(ws(',')) ~ ws(')') ~> (ConfigList(_)) }

  def True = rule { wsi("true") ~ push(ConfigBoolean(true)) }
  def False = rule { wsi("false") ~ push(ConfigBoolean(false)) }
  def Number = rule { Float | Long | Int }
  def Long = rule { capture(HexInteger | Integer) ~> (ConfigLong(_)) ~ "L" ~ WhiteSpace }
  def Int = rule { capture(HexInteger | Integer) ~> (ConfigInt(_)) ~ WhiteSpace }
  def Float = rule { capture(Integer ~ ((DecFrac ~ optional(DecExp)) | DecExp)) ~> (ConfigFloat(_)) ~ WhiteSpace }
  def String = rule { StringUnwrapped ~> (ConfigString(_)) }

  def Identifier = rule { capture(IdAlpha ~ zeroOrMore(IdAlphaNum)) ~> (_.toString) ~ WhiteSpace }
  def StringUnwrapped = rule { '"' ~ clearSB() ~ Characters ~ ws('"') ~ push(sb.toString) }

  def Characters = rule { zeroOrMore(NormalChar | '\\' ~ EscapedChar) }
  def EscapedChar = rule (
    QuoteSlashBackSlash ~ appendSB()
      | 'b' ~ appendSB('\b')
      | 'f' ~ appendSB('\f')
      | 'n' ~ appendSB('\n')
      | 'r' ~ appendSB('\r')
      | 't' ~ appendSB('\t')
      | Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
  )
  def NormalChar = rule { !QuoteBackslash ~ ANY ~ appendSB() }
  def Unicode = rule { 'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16)) }

  def DecExp = rule { ignoreCase("e") ~ optional(anyOf("+-")) ~ Digits }
  def DecFrac = rule { "." ~ Digits }

  def HexInteger = rule { "0x" ~ HexDigits }
  def HexDigits = rule { oneOrMore(HexDigit) }
  def HexDigit = rule { "0" - "9" | "a" - "f" | "A" - "F" }

  def Integer = rule { optional(PlusMinus) ~ (Digit19 ~ Digits | Digit) }
  def Digits = rule { oneOrMore(Digit) }

  def WhiteSpace: Rule0 = rule { zeroOrMore(oneOrMore(WhiteSpaceChar) | CppComment | LineComment) }
  def CppComment: Rule0 = rule { "/*" ~ zeroOrMore(!"*/" ~ ANY) ~ "*/" }
  def LineComment: Rule0 = rule { ("#" | "//") ~ zeroOrMore(!EOLChar ~ ANY) ~ ("\r\n" | "\r" | "\n" | EOI) }

  def ws(c: Char) = rule { c ~ WhiteSpace }
  def ws(s: String) = rule { s ~ WhiteSpace }
  def wsi(s: String) = rule { ignoreCase(s) ~ WhiteSpace }
}

object ConfigParser {
  def apply(text: String): ConfigGroup = {
    import Parser.DeliveryScheme.Try

    val parser = new ConfigParser(text)
    parser.Root.run() match {
      case Success(cg) => cg
      case Failure(e: ParseError) => throw new RuntimeException(parser.formatError(e))
      case Failure(e) => throw e
    }
  }
}

object App {
  def main(args: Array[String]) {
    import com.alexknvl.libconfig.conv._
    import com.alexknvl.libconfig.conv.DefaultProtocol._
    import com.alexknvl.libconfig.ast.Getters._

    val config = ConfigParser("""
      # Example application configuration file

      version = "1.0";

      application:
      {
        window:
        {
          title = "My Application";
          size = { w = 640; h = 480; };
          pos = { x = 350; y = 250; };
        };

        list = ( ( "abc", 123 ), 1.234, ( /* an empty list */) );

        books = ( { title  = "Treasure Island";
                   author = "Robert Louis Stevenson";
                   price  = 29.95;
                   qty    = 5; },
                 { title  = "Snow Crash";
                   author = "Neal Stephenson";
                   price  = 9.99;
                   qty    = 8; } );

        misc:
        {
          pi = 3.141592654;
          bigint = 9223372036854775807L;
          columns = [ "Last Name", "First Name", "MI" ];
          bitmask = 0x1FC3;
        };
      };
      """)

    println(config("version").to[String])
    println(config("application.window.title").to[String])
    println(config("application.list.[0]").to[(String, Int)])
  }
}
