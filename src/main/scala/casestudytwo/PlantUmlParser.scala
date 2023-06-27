package casestudytwo

import casestudytwo.*
import casestudytwo.ScalaToPlantUml.mapPlantUmlToPlantUmlString

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

object PlantUmlParser extends JavaTokenParsers {

  def apply(plantUml: String): PlantUml = {
    parse(plantUmlParser, plantUml) match
      case Success(matched, r) => matched
      case Failure(msg, r) => throw Exception(s"FAILURE: $msg at ${r.pos}")
      case Error(msg, r) => throw Exception(s"ERROR: $msg at ${r.pos}")
  }

  private val singleWordPattern: PlantUmlParser.Parser[String] = """\w+""".r
  
  private val privateParser: PlantUmlParser.Parser[Access] = """-""".r ^^^ Access._private
  private val publicParser: PlantUmlParser.Parser[Access] = """\+""".r ^^^ Access._public
  private val protectedParser: PlantUmlParser.Parser[Access] = """#""".r ^^^ Access._protected
  private val packagePrivateParser: PlantUmlParser.Parser[Access] = """~""".r ^^^ Access._packagePrivate
  private val accessParser: PlantUmlParser.Parser[Access] = privateParser | publicParser | protectedParser | packagePrivateParser

  private val methodTypeParser = singleWordPattern.withFailureMessage("Single word for method type expected.")
    .withErrorMessage("Single word for method type expected.")
  private val paramTypeParser = singleWordPattern.withFailureMessage("Single word for parameter type expected.")
    .withErrorMessage("Single word for parameter type expected.")
  private val fieldNameParser = singleWordPattern.withFailureMessage("Single word for field name expected.")
    .withErrorMessage("Single word for field name expected.")
  private val methodNameParser = singleWordPattern.withFailureMessage("Single word for method name expected.")
    .withErrorMessage("Single word for method name expected.")
  private val conceptNamePattern = singleWordPattern.withFailureMessage("Single word for concept name expected.")
    .withErrorMessage("Single word for concept name expected.")

  val parameterParser: PlantUmlParser.Parser[Parameter] = fieldNameParser ~ """:""".r ~ paramTypeParser ^^ {
    case fieldName ~ colon ~ _type => Parameter(fieldName, _type)
  }

  private val abstractParser = "{abstract}" ^^^ NonAccessModifier.ABSTRACT
  private val staticParser = "{static}" ^^^ NonAccessModifier.STATIC
  private val nonAccessModifier: PlantUmlParser.Parser[NonAccessModifier] = (abstractParser | staticParser)
    .withFailureMessage("Non access modifier expected.").withErrorMessage("Non access modifier expected.")

  val methodParser: PlantUmlParser.Parser[Method] = accessParser ~ opt(nonAccessModifier) ~ methodNameParser
    ~ """(""" ~ opt(repsep(parameterParser, ",")) ~ """):""" ~ methodTypeParser ^^ {
    case access ~ nonAccessModifierOpt ~ methodName ~ openBracket ~ parameterOpt ~ closingBracketString ~ _type =>
      Method(access, methodName, _type, parameterOpt.getOrElse(ListBuffer().toList), nonAccessModifierOpt.orNull)
  }

  val fieldParser: PlantUmlParser.Parser[Field] = accessParser ~ opt(nonAccessModifier) ~ parameterParser ^^ {
    case access ~ nonAccessModifierOpt ~ parameter =>
      Field(access, parameter.name, parameter.`type`, nonAccessModifierOpt.orNull)
  }

  val constructorParser: PlantUmlParser.Parser[Constructor] =
    accessParser ~ conceptNamePattern ~ """(""" ~ opt(repsep(parameterParser, ",")) ~ """)""" ~ """<<Constructor>>""" ^^ {
      case access ~ className ~ openBracket ~ parameterOpt ~ closingBracketString ~ constructorString =>
        Constructor(access, parameterOpt.getOrElse(ListBuffer().toList))
    }

  private val classConceptsParser = fieldParser | methodParser | constructorParser

  private val classBodyParser: PlantUmlParser.Parser[Entity] =
    """{""" ~> rep(classConceptsParser) <~ """}""" ^^ { bodyConcepts =>
      Entity("", SemanticModelBuilder.getFields(bodyConcepts), SemanticModelBuilder.getMethods(bodyConcepts),
        SemanticModelBuilder.getConstructors(bodyConcepts), false, false)
    }


  private val classParser: Parser[Entity] =
    """class """ ~> conceptNamePattern ~ opt(classBodyParser) ^^ { case className ~ classBodyOpt =>
      classBodyOpt.getOrElse(Entity(className, List.empty, List.empty, List.empty, false, false)).copy(name = className)
    }

  private val abstractClassParser: Parser[Entity] =
    """abstract """ ~> classParser ^^ (clazz => clazz.copy(isAbstract = true))

  private val interfaceConceptBodyParser = fieldParser | methodParser

  private val interfaceBodyParser: PlantUmlParser.Parser[Entity] =
    """{""" ~> rep(interfaceConceptBodyParser) <~ """}""" ^^ {
      bodyConcepts =>
        Entity("", SemanticModelBuilder.getFields(bodyConcepts), SemanticModelBuilder.getMethods(bodyConcepts), List.empty, false, true)
    }

  private val interfaceParser: Parser[Entity] =
    """interface """ ~> conceptNamePattern ~ opt(interfaceBodyParser) ^^ {
      case interfaceName ~ clazz =>
        clazz.getOrElse(Entity(interfaceName, List.empty, List.empty, List.empty, false, true)
        ).copy(name = interfaceName, isInterface = true)
    }


  private val enumValuesParser: Parser[List[String]] = """{""" ~> rep(singleWordPattern) <~ """}"""
  private val enumParser: Parser[Enumeration] =
    """enum """ ~> conceptNamePattern.withFailureMessage("Single word for enum name expected.") ~ opt(enumValuesParser) ^^ {
      case className ~ enumValues => Enumeration(className, enumValues.getOrElse(List.empty))
    }

  private val extension: PlantUmlParser.Parser[DependencyType] = "<|--" ^^^ DependencyType.EXTENSION
  private val composition: PlantUmlParser.Parser[DependencyType] = "*--" ^^^ DependencyType.COMPOSITION
  private val aggregation: PlantUmlParser.Parser[DependencyType] = "o--" ^^^ DependencyType.AGGREGATION
  private val dependencyType: PlantUmlParser.Parser[DependencyType] = extension | composition | aggregation

  private val cardinalityParser = """"[^"]+"""".r ^^ (cardinality => cardinality.substring(1, cardinality.length - 1))
  private val dependencyFromParser: PlantUmlParser.Parser[DependencyConcept] = singleWordPattern ~ opt(cardinalityParser) ^^ {
    case className ~ cardinality => DependencyConcept(className, cardinality.getOrElse(""))
  }
  private val dependencyToParser: PlantUmlParser.Parser[DependencyConcept] = opt(cardinalityParser) ~ singleWordPattern ^^ {
    case cardinality ~ className => DependencyConcept(className, cardinality.getOrElse(""))
  }
  private val labelParser: PlantUmlParser.Parser[String] = ":" ~> """.*""".r

  private val dependencyParser: PlantUmlParser.Parser[Dependency] =
    dependencyFromParser ~ dependencyType ~ dependencyToParser ~ opt(labelParser) ^^ {
      case from ~ dependencyType ~ to ~ label => Dependency(from, to, dependencyType, label.getOrElse(""))
    }

  private val conceptParser: PlantUmlParser.Parser[Object] = classParser | abstractClassParser | interfaceParser | enumParser | dependencyParser

  private val plantUmlParser: Parser[PlantUml] =
    """@startuml""" ~> rep(conceptParser) <~ """@enduml""" ^^ (concepts => SemanticModelBuilder.buildPlantUml(concepts))
}