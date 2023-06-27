package casestudyone

import casestudyone.Constants.{ANYTHING_PATTERN, ANY_LETTERS_PATTERN, ANY_NUMBER_PATTERN, SOMETHING_PATTERN}
import casestudyone.RegexParser.{parse, regexParser}
import org.scalatest.funsuite.AnyFunSuite

import scala.util.matching.Regex

class RegexParserTest extends AnyFunSuite {

  // _________________ starts with _________________
  test("starts with anything") {
    // GIVEN
    val regexDsl = """starts with anything"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^(".concat(ANYTHING_PATTERN + ")"))
    assert(result.matches("Should match."))
    assert(result.matches(""))
  }

  test("starts with something") {
    // GIVEN
    val regexDsl = """starts with something"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^(".concat(SOMETHING_PATTERN + ")"))
    assert(result.matches("Should match."))
    assert(!result.matches(""))
  }

  test("starts with letters") {
    // GIVEN
    val regexDsl =
      """starts with letters"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == s"${"^(".concat(ANY_LETTERS_PATTERN)})")
    assert(result.matches("Should match"))
    assert(!result.matches("123"))
  }

  test("starts with numbers") {
    // GIVEN
    val regexDsl = """starts with numbers"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^(".concat(ANY_NUMBER_PATTERN + ")"))
    assert(result.matches("123"))
    assert(!result.matches("Test"))
  }

  test("starts with Hello") {
    // GIVEN
    val regexDsl = """starts with "Hello""""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^".concat("(Hello)"))
    assert(result.matches("Hello"))
    assert(!result.matches("hello"))
  }

  test("starts with @") { // new
    // GIVEN
    val regexDsl = """starts with "@""""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^".concat("(@)"))
    assert(result.matches("@"))

  }

  test("starts with first or second") {
    // GIVEN
    val regexDsl = """starts with "first" or "second""""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^".concat("(first|second)"))
    assert(result.matches("first"))
    assert(result.matches("second"))
    assert(!result.matches("third"))
    assert(!result.matches("fourth"))

  }

  test("starts with whitespace between words") {
    // GIVEN
    val regexDsl =
      """starts with "Hello World!" or "Hello""""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^".concat("(Hello World!|Hello)"))
    assert(result.matches("Hello World!"))
    assert(result.matches("Hello"))
    assert(!result.matches("Test"))
  }

  // _________________ followed with _________________

  test("followed with anything") {
    // GIVEN
    val regexDsl = """followed with anything"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "(%s)".format(ANYTHING_PATTERN))
    assert(result.matches("Should match."))
    assert(result.matches(""))

  }

  test("followed with something") {
    // GIVEN
    val regexDsl =
      """followed with something"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "(%s)".format(SOMETHING_PATTERN))
    assert(result.matches("Should match."))
    assert(!result.matches(""))

  }

  test("followed with letters") {
    // GIVEN
    val regexDsl =
      """followed with letters"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "(%s)".format(ANY_LETTERS_PATTERN))
    assert(result.matches("Should match"))
    assert(!result.matches("123"))

  }

  test("followed with numbers") {
    // GIVEN
    val regexDsl =
      """followed with numbers"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "(%s)".format(ANY_NUMBER_PATTERN))
    assert(result.matches("123"))
    assert(!result.matches("Test"))

  }

  test("followed with @") {
    // GIVEN
    val regexDsl = """followed with "@""""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "(@)")
    assert(result.matches("@"))
    assert(!result.matches("@@"))
  }

  test("followed with @ or email") {
    // GIVEN
    val regexDsl =
      """followed with "@" or "email""""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "(@|email)")
    assert(result.matches("@"))
    assert(result.matches("email"))
    assert(!result.matches("@email"))
    assert(!result.matches("emailtest"))
  }

  test("followed with multiple times in correct order") {
    // GIVEN
    val regexDsl =
      """
          followed with "gmail" or "gmx"
          followed with "."
          followed with "com" or "de"
          """.stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "(gmail|gmx)(\\.)(com|de)")
    assert(result.matches("gmail.com"))
    assert(result.matches("gmail.de"))
    assert(result.matches("gmx.com"))
    assert(result.matches("gmx.de"))
    assert(!result.matches("gmxde"))
    assert(!result.matches("gmailgmx.de"))
    assert(!result.matches("gmail."))
  }

  // _________________ ends with _________________

  test("ends with anything") {
    // GIVEN
    val regexDsl =
      """ends with anything"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == s"(${ANYTHING_PATTERN.concat(")$")}")
    assert(result.matches("Should match."))
    assert(result.matches(""))


  }

  test("ends with something") {
    // GIVEN
    val regexDsl =
      """ends with something"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == s"(${SOMETHING_PATTERN.concat(")$")}")
    assert(result.matches("Should match."))
    assert(!result.matches(""))
  }

  test("ends with letters") {
    // GIVEN
    val regexDsl =
      """ends with letters"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == s"(${ANY_LETTERS_PATTERN.concat(")$")}")
    assert(result.matches("Should match"))
    assert(!result.matches("123"))


  }

  test("ends with numbers") {
    // GIVEN
    val regexDsl =
      """ends with numbers"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == s"(${ANY_NUMBER_PATTERN.concat(")$")}")
    assert(result.matches("123"))
    assert(!result.matches("Test"))


  }

  test("ends with Hello") {
    // GIVEN
    val regexDsl =
      """ends with "Hello""""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "(Hello)".concat("$"))
    assert(result.matches("Hello"))
    assert(!result.matches("Hello World"))


  }

  test("ends with first or second") {
    // GIVEN
    val regexDsl =
      """ends with "first" or "second""""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "(first|second)".concat("$"))
    assert(result.matches("first"))
    assert(result.matches("second"))
    assert(!result.matches("third"))
    assert(!result.matches("fourth"))


  }

  // _________________ Multiplicities  _________________

  test("starts with quantification") {
    // GIVEN
    val regexDsl =
      """starts with "aa" occurs 1..2 or "b" occurs 3..3"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^((aa){1,2}|(b){3})")
    assert(result.matches("aa"))
    assert(result.matches("aaaa"))
    assert(result.matches("bbb"))
    assert(!result.matches("aaa"))
    assert(!result.matches("bb"))
    assert(!result.matches("b"))


  }

  test("followed with repeated and multiplicities") {
    // GIVEN
    val regexDsl =
      """ followed with "gmail" occurs 1..2 or "gmx"
          followed with "." occurs 3..3
          followed with "com" or "de"""".stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "((gmail){1,2}|gmx)((\\.){3})(com|de)")
    assert(result.matches("gmailgmail...com"))
    assert(result.matches("gmail...de"))
    assert(result.matches("gmx...com"))
    assert(result.matches("gmx...de"))
    assert(!result.matches("gmxde"))
    assert(!result.matches("gmailgmx.de"))
    assert(!result.matches("gmail."))
    assert(!result.matches("gmail."))
  }

  test("ends with quantification") {
    // GIVEN
    val regexDsl =
      """ends with "aa" occurs 1..2 or "b" occurs 3..3"""

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "((aa){1,2}|(b){3})$")
    assert(result.matches("aa"))
    assert(result.matches("aaaa"))
    assert(result.matches("bbb"))
    assert(!result.matches("aaa"))
    assert(!result.matches("bb"))
    assert(!result.matches("b"))
  }

  test("ends with quantification indefinitely") {
    val regexDsl =
      """ends with "aa" occurs indefinitely or "b" occurs 3..3"""
    val result: Regex = RegexParser(regexDsl)


    assert(result.pattern.pattern() == "((aa)*|(b){3})$")
    assert(result.matches("aaaaaaaaaa"))
    assert(result.matches("aaaa"))
    assert(result.matches("bbb"))
    assert(!result.matches("aaa"))
    assert(!result.matches("bb"))
    assert(!result.matches("b"))
  }

  // _________________ integration _________________

  test("simple regex") {
    // GIVEN
    val regexDsl =
      """ starts with "T"
          followed with anything
          ends with "s."""".stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^(T)(.*)(s\\.)$")
    assert(result.matches("This matches."))
    assert(!result.matches("This does not match."))
  }

  test("simple regex 2") {
    // GIVEN
    val regexDsl =
      """ starts with "This m"
            followed with "atche"
            ends with "s."""".stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^(This m)(atche)(s\\.)$")
    assert(result.matches("This matches."))
    assert(!result.matches("Does not match."))
  }

  test("test email regex") {
    // GIVEN
    val regexDsl =
      """ starts with something
          followed with "@"
          followed with something
          followed with "."
          ends with "com" or "de" or "net"""".stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern == "^(.+)(@)(.+)(\\.)(com|de|net)$")
    assert(result.matches("name@gmail.com"))
    assert(result.matches("name@gmx.de"))
    assert(result.matches("name@gmx.de"))
    assert(result.matches("name@gmx.net"))
    assert(!result.matches("@gmx.de"))
    assert(!result.matches("name@.de"))
    assert(!result.matches("name@.ch"))
    assert(!result.matches("name.de"))
  }

  // _________________ inner regex _________________

  test("Inner regex") {
    // GIVEN
    val regexDsl =
      """
        starts with inner regex(followed with "John" or "Steve") or "Hello"
        followed with "Doe" or "Mustermann"
        followed with "@"
        ends with "gmail.com"
        """.stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^((John|Steve)|Hello)(Doe|Mustermann)(@)(gmail\\.com)$")
    assert(result.matches("JohnDoe@gmail.com"))
    assert(result.matches("JohnMustermann@gmail.com"))
    assert(result.matches("SteveMustermann@gmail.com"))
    assert(result.matches("HelloMustermann@gmail.com"))
    assert(!result.matches("JohnD@gmail.com"))
    assert(!result.matches("Demian@gmail.com"))
  }

  test("Inner regex 2") {
    // GIVEN
    val regexDsl =
      """starts with inner regex(followed with "John")
      followed with "Doe" or "Mustermann"
      followed with inner regex(followed with "a" followed with "b" or "c")
      ends with "end"
        """.stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^((John))(Doe|Mustermann)((a)(b|c))(end)$")
    assert(result.matches("JohnDoeabend"))
    assert(result.matches("JohnMustermannacend"))
    assert(!result.matches("JohnMustermannaend"))
  }

  test("Inner regex followed with with alternative") {
    // GIVEN
    val regexDsl =
      """starts with inner regex(followed with "1" or "2") or "4"
      followed with inner regex(followed with "a" followed with "b" or "c")
      ends with "end"
        """.stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^((1|2)|4)((a)(b|c))(end)$")
    assert(result.matches("1abend"))
    assert(result.matches("2abend"))
    assert(result.matches("4acend"))
    assert(!result.matches("4aend"))
  }

  // _________________ semantic validation _________________

  test("Quantification is negative") {
    // GIVEN
    val regexDsl =
      """starts with "first" occurs 1..-1"""".stripMargin


    // WHEN
    val caught =
      intercept[Exception] {
        RegexParser(regexDsl)
      }

    // THEN
    assert(caught.getMessage == "ERROR: Quantification must be positive for: 1..-1 at 1.33")
  }

  test("Quantification is in wrong order") {
    // GIVEN
    val regexDsl =
      """starts with "first" occurs 2..1""".stripMargin

    // WHEN
    val caught =
      intercept[Exception] {
        RegexParser(regexDsl)
      }

    // THEN
    assert(caught.getMessage == "ERROR: First value must be smaller than second value for: 2..1 at 1.32")
  }

  // --------------- Parser Combinator specific ---------------

  test("Missing pre keywords") {
    // GIVEN
    val regexDsl =
      """"4" or "5"""".stripMargin

    // WHEN
    val caught = intercept[Exception] {
      RegexParser(regexDsl)
    }

    // THEN
    assert(caught.getMessage == """ERROR: Prefix is missing near "4" or "5" at 1.11""")
  }

  test("Missing pre keywords in second line") {
    // GIVEN
    val regexDsl =
      """starts with "4" or "5"
          "6 3" or "7"""".stripMargin

    // WHEN
    val caught = intercept[Exception] {
      RegexParser(regexDsl)
    }

    // THEN
    assert(caught.getMessage == """ERROR: Prefix is missing near "6 3" or "7" at 2.23""")
  }

  test("Wrong pre keyword in second line") {
    // GIVEN
    val regexDsl =
      """starts with something
        followed with "@"
        folowed with something
        ends with "com" or "de" or "net"""".stripMargin

    // WHEN
    val caught = intercept[Exception] {
      RegexParser(regexDsl)
    }

    // THEN
    assert(caught.getMessage == """ERROR: Prefix is missing near folowed with something at 4.9""")
  }

  // --------------- Missing double quotes ---------------

  test("Missing double quotes at start of first") {
    // GIVEN
    val regexDsl =
      """starts with 4" or "5"""".stripMargin

    // WHEN
    val caught = intercept[Exception] {
      RegexParser(regexDsl)
    }

    // THEN
    assert(caught.getMessage == """ERROR: Missing double quotes near 4" or "5" at 1.22""")
  }

  test("Missing double quotes at end of first") {
    // GIVEN
    val regexDsl = """starts with "4 or "5"""".stripMargin

    // WHEN
    val caught = intercept[Exception] {
      RegexParser(regexDsl)
    }

    // THEN
    assert(caught.getMessage == """ERROR: Prefix is missing near 5" at 1.22""")
  }

  test("Missing double quotes at start of second") {
    // GIVEN
    val regexDsl =
      """starts with "4" or 5"""".stripMargin

    // WHEN
    val caught = intercept[Exception] {
      RegexParser(regexDsl)
    }

    // THEN
    assert(caught.getMessage == """ERROR: Missing double quotes near 5" at 1.22""")
  }

  test("Missing double quotes at end of second") {
    // GIVEN
    val regexDsl = """starts with "4" or "5""".stripMargin

    // WHEN
    val caught = intercept[Exception] {
      RegexParser(regexDsl)
    }

    // THEN
    assert(caught.getMessage == """ERROR: Missing double quotes near "5 at 1.22""")
  }

  test("Missing all double quotes") {
    // GIVEN
    val regexDsl = """starts with 4 or 5""".stripMargin
    val s = ""
    // WHEN
    val caught =
      intercept[Exception] {
        RegexParser(regexDsl)
      }

    // THEN
    assert(caught.getMessage == "ERROR: Missing double quotes near 4 or 5 at 1.19")
  }

  // --------------- extra tests ---------------

  test("or inside double quotes is not interpreted as splitting term") {
    // GIVEN
    val regexDsl = """starts with "or" or "else" or "if"""".stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)

    // THEN
    assert(result.pattern.pattern() == "^(or|else|if)")
    assert(result.matches("or"))
    assert(result.matches("else"))
    assert(result.matches("if"))
    assert(!result.matches("orelse"))
    assert(!result.matches("Test"))
  }


  test("for loc") {
    // GIVEN
    val regexDsl =
      """starts with "domain "
     followed with inner regex(followed with "specific " followed with inner regex(followed with "modeling" or "design")) or "driven design"
     ends with anything""".stripMargin

    // WHEN
    val result: Regex = RegexParser(regexDsl)
    // THEN
    assert(result.pattern.pattern() == "^(domain )((specific )((modeling|design))|driven design)(.*)$")
  }
}
