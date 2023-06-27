package casestudyone

final case class SyntaxError(private val message: String = "") extends Exception(message)