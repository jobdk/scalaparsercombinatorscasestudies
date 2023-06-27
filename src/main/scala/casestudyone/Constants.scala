package casestudyone

object Constants {
  val ANYTHING_PATTERN: String = ".*"
  val SOMETHING_PATTERN: String = ".+"
  val ANY_LETTERS_PATTERN: String = """[a-zA-ZäÄüÜöÖß\s]+"""
  val ANY_NUMBER_PATTERN: String = "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?"
  val INDEFINITELY: String = "indefinitely"
}