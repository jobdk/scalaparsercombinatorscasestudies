package casestudyone

import casestudyone.Constants.{ANYTHING_PATTERN, ANY_LETTERS_PATTERN, ANY_NUMBER_PATTERN, SOMETHING_PATTERN}
import casestudyone.RegexParser.{err, success}
import casestudyone.SemanticModelBuilder.buildFollowsWith
import casestudyone.Validation

import java.text.NumberFormat
import scala.util.matching.*
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

object RegexParser extends RegexParsers {
  def apply(regexDsl: String): Regex = {
    parse(regexParser, regexDsl) match
      case Success(matched, _) => matched
      case Failure(msg, r) => throw Exception(s"FAILURE: $msg at ${r.pos}")
      case Error(msg, r) => throw Exception(s"ERROR: $msg at ${r.pos}")
  }

  private lazy val regexParser: RegexParser.Parser[Regex] = opt(startsWithParser) ~ opt(rep(followedWithParser)) ~ opt(endsWithParser) ^^ {
    case startsWithOpt ~ followedWithOpt ~ endsWithOpt ⇒ SemanticModelBuilder.buildRegex(startsWithOpt, followedWithOpt, endsWithOpt)
  }

  private val SPLITT_OR: String = "or"
  private lazy val STARTS_WITH_PATTERN: String = "starts with"
  private lazy val startsWithParser: RegexParser.Parser[String] = handleMissingPrefixParser | STARTS_WITH_PATTERN
    ~> repsep(splitOccursParser, SPLITT_OR) ^^ { value => SemanticModelBuilder.buildStartsWith(value) }
  
  private lazy val FOLLOWED_WITH_PATTERN: String = "followed with"
  lazy val followedWithParser: RegexParser.Parser[String] = handleMissingPrefixParser | FOLLOWED_WITH_PATTERN
    ~> repsep(splitOccursParser, SPLITT_OR) ^^ SemanticModelBuilder.buildFollowsWith
  
  private lazy val ENDS_WITH_PATTERN: String = "ends with"
  lazy val endsWithParser: RegexParser.Parser[String] =
    handleMissingPrefixParser | ENDS_WITH_PATTERN ~> repsep(splitOccursParser, SPLITT_OR) ^^ SemanticModelBuilder.buildEndsWith
  
  private lazy val INNER_REGEX_PATTERN: String = """inner regex"""
  private lazy val innerRegexParser: RegexParser.Parser[String] =
    INNER_REGEX_PATTERN ~ "(" ~> rep(followedWithParser) <~ ")" ^^ SemanticModelBuilder.buildInnerRegex
  
  private val anythingParser: Parser[String] = """anything""" ^^^ {
    ANYTHING_PATTERN
  }
  private lazy val somethingParser: RegexParser.Parser[String] = """something""" ^^^ {
    SOMETHING_PATTERN
  }
  private lazy val lettersParser: RegexParser.Parser[String] = """letters""" ^^^ {
    ANY_LETTERS_PATTERN
  }
  private lazy val numbersParser: RegexParser.Parser[String] = """numbers""" ^^^ {
    ANY_NUMBER_PATTERN
  }
  
  private lazy val preDefinedTermParser: Parser[String] = anythingParser | somethingParser | lettersParser | numbersParser
   
  private lazy val termParser: Parser[String] = preDefinedTermParser |
    """"[^"]+"""".r ^^ {
      value => SemanticModelBuilder.mapTermToCorrectValue(value)
    }
   
  private lazy val correctQuantificationParser = """(\d+)\.\.(\d+)""".r >> Validation.isQuantificationWrongOrder
  private lazy val indefinitelyParser: Parser[String] = """indefinitely""".r
  private lazy val quantificationParser: Parser[String] = correctQuantificationParser | indefinitelyParser | handleWrongQuantification

  private val SPLITT_OCCURS = "occurs"
  private lazy val splitOccursParser: RegexParser.Parser[String] = {
    repsep(innerRegexParser | termParser
      | quantificationParser | handleMissingDoubleQuote, SPLITT_OCCURS) ^^ SemanticModelBuilder.buildTerm
  }

  private lazy val invalidSentence: Parser[String] = """.+""".r
  private lazy val handleMissingDoubleQuote: RegexParser.Parser[String] = invalidSentence >> { value =>
    err(s"Missing double quotes near ${value.mkString}")
  }
  
  private lazy val handleWrongQuantification: RegexParser.Parser[String] =
    """(-?\d+)\.\.(-?\d+)""".r >> Validation.quantificationHasNegativeValue

  private lazy val missingPrefixParser: RegexParser.Parser[String] =
    s"""^(?!$STARTS_WITH_PATTERN|$FOLLOWED_WITH_PATTERN|$ENDS_WITH_PATTERN|\\)).+""".r
  private lazy val handleMissingPrefixParser: RegexParser.Parser[String] =
    missingPrefixParser >> { value => err(s"Prefix is missing near ${value.mkString}") }
}

