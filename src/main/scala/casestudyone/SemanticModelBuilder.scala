package casestudyone

import casestudyone.Constants

import scala.util.matching.Regex

object SemanticModelBuilder {
  def buildStartsWith(value: List[String]): String = s"^(${value.mkString("|")}${")"}"

  def buildFollowsWith(value: List[String]): String = s"(${value.mkString("|")})"

  def buildEndsWith(value: List[String]): String = s"(${value.mkString("|")})$$"

  def buildTerm(contentList: List[String]): String =
    contentList match {
      case List(head) => head
      case List(head, tail) =>
        val quantification: Array[String] = tail.split("\\.\\.")
        if (isIndefinitely(quantification)) return s"""($head)*"""
        if (isTheSame(quantification)) return s"""($head){${quantification.head}}"""
        "(%s){%s,%s}".format(head, quantification.head, quantification.last)
      case _ => throw new IllegalArgumentException("Invalid input: contentList must have 1 or 2 elements")
    }


  private def isIndefinitely(quantification: Array[String]) = {
    quantification.last == Constants.INDEFINITELY
  }

  private def isTheSame(quantification: Array[String]) = {
    quantification.last == quantification.head
  }

  def buildInnerRegex(value: List[String]): String = {
    "%s".format(value.mkString(""))
  }

  def buildRegex(startsWithOpt: Option[String], followedWithOpt: Option[List[String]], endsWithOpt: Option[String]): Regex =
    new Regex(startsWithOpt.getOrElse("") + followedWithOpt.get.mkString + endsWithOpt.getOrElse(""))

  def mapTermToCorrectValue(term: String): String = {
    term.drop(1).dropRight(1).replace(".", "\\.")
  }
}



