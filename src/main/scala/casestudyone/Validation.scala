package casestudyone

import casestudyone.Constants.{ANYTHING_PATTERN, ANY_LETTERS_PATTERN, ANY_NUMBER_PATTERN, SOMETHING_PATTERN}
import RegexParser.{err, success}

import java.text.NumberFormat
import scala.util.matching.*
import scala.util.parsing.combinator.JavaTokenParsers

object Validation {
  def quantificationHasNegativeValue(value: String): RegexParser.Parser[Nothing] = {
    val quantification: Array[String] = value.split("\\.\\.")
    err(if (quantification.head.toInt < 0 || quantification.last.toInt < 0) s"Quantification must be positive for: $value"
    else s"Something went wrong for:  $value")  }

  def isQuantificationWrongOrder(value: String): RegexParser.Parser[String] = {
    val quantification: Array[String] = value.split("\\.\\.")
    if (quantification.head.toInt > quantification.last.toInt)
      err(s"First value must be smaller than second value for: $value") else success(value)  }}
