package casestudytwo

import scala.collection.mutable.ListBuffer

object ScalaToPlantUml {

  def mapPlantUmlToPlantUmlString(plantUml: PlantUml): String = {
    ("@startuml\n"
      + mapClassToClassString(plantUml.classes)
      + mapAbstractClassToAbstractClassString(plantUml.abstractClasses)
      + mapEnumToEnumString(plantUml.enums)
      + mapInterfaceToInterfaceString(plantUml.interfaces)
      + mapDependenciesToDependencyString(plantUml.dependencies)
      + "\n@enduml").stripMargin
  }

  private def mapClassToClassString(classes: List[Entity]): String = {
    if (classes.isEmpty) return ""
    val result = ListBuffer[String]()
    classes.map { clazz =>
      val builder = new StringBuilder
      if (clazz.methods.isEmpty && clazz.fields.isEmpty && clazz.constructors.isEmpty) {
        builder.append(s"class ${clazz.name}")
      } else {
        builder.append(s"class ${clazz.name} {\n")
          .append(mapFieldsToFieldString(clazz.fields) + "\n")
          .append(mapConstructorsToConstructorString(clazz.constructors, clazz.name) + "\n")
          .append(mapMethodsToMethodString(clazz.methods))
          .append("\n")
          .append("}")
      }
      result.addOne(builder.toString())
    }
    result.mkString("\n") + "\n"
  }

  private def mapInterfaceToInterfaceString(interface: List[Entity]): String = {
    if (interface.isEmpty) return ""
    val result = ListBuffer[String]()
    interface.map { interface =>
      val builder = new StringBuilder
      if (interface.methods.isEmpty && interface.fields.isEmpty) {
        builder.append(s"interface ${interface.name}")
      } else {
        builder.append(s"interface ${interface.name} {\n")
          .append(mapFieldsToFieldString(interface.fields) + "\n")
          .append(mapMethodsToMethodString(interface.methods) + "\n")
          .append("}")
      }
      result.addOne(builder.toString())
    }
    result.mkString("\n") + "\n"
  }

  private def mapEnumToEnumString(enums: List[Enumeration]): String = {
    if (enums.isEmpty) return ""
    val result = ListBuffer[String]()
    enums.map { enumeration =>
      val builder = new StringBuilder
      if (enumeration.values.isEmpty) {
        builder.append(s"enum ${enumeration.name}")
      } else {
        builder.append(s"enum ${enumeration.name} {\n")
          .append(enumeration.values.mkString("\n"))
          .append("\n")
          .append("}")
      }
      result.addOne(builder.toString())
    }
    result.mkString("\n") + "\n"
  }

  private def mapAbstractClassToAbstractClassString(abstractClasses: List[Entity]): String = {
    if (abstractClasses.isEmpty) return ""
    val result = ListBuffer[String]()
    abstractClasses.map { abstractClass =>
      val builder = new StringBuilder
      if (abstractClass.methods.isEmpty && abstractClass.fields.isEmpty && abstractClass.constructors.isEmpty) {
        builder.append(s"abstract class ${abstractClass.name}")
      } else {
        builder.append(s"abstract class ${abstractClass.name} {\n")
          .append(mapFieldsToFieldString(abstractClass.fields) + "\n")
          .append(mapConstructorsToConstructorString(abstractClass.constructors, abstractClass.name) + "\n")
          .append(mapMethodsToMethodString(abstractClass.methods) + "\n")
          .append("}")
      }
      result.addOne(builder.toString())
    }
    result.mkString("\n") + "\n"
  }

  private def mapFieldsToFieldString(fields: List[Field]): String = {
    if (fields.isEmpty) return ""
    fields.map(field => s"${accessModifier.getOrElse(field.access, "")}${nonAccessModifier.getOrElse(field.nonAccessModifier, "")} ${field.name}: ${field.`type`}").mkString("\n")
  }

  private def mapMethodsToMethodString(methods: List[Method]): String = {
    if (methods.isEmpty) return ""
    methods.map(method =>
      s"${accessModifier.getOrElse(method.access, "")}${nonAccessModifier.getOrElse(method.nonAccessModifier, "")} ${method.name}(${mapParametersToParameterString(method.parameters)}): ${method.`type`}"
    ).mkString("\n")
  }

  private def mapParametersToParameterString(parameters: List[Parameter]): String = {
    parameters.map(parameter => s"${parameter.name}: ${parameter.`type`}").mkString(", ")
  }

  private def mapConstructorsToConstructorString(constructors: List[Constructor], className: String): String = {
    if (constructors.isEmpty) return ""
    constructors.map(constructor => s"${accessModifier.getOrElse(constructor.access, "")} $className(${mapParametersToParameterString(constructor.parameters)}) <<Constructor>>").mkString("\n")
  }

  private def getCardinality(str: String): String = if (str.nonEmpty) s" \"$str\" " else " "

  private def getLabel(str: String): String = if (str.nonEmpty) s" : $str" else ""

  private def mapDependenciesToDependencyString(dependencies: List[Dependency]): String = {
    if (dependencies.isEmpty) return ""
    dependencies.map(dependency => s"${dependency.from.name}${getCardinality(dependency.from.cardinality)}${dependencyTypes(dependency.dependencyType)}${getCardinality(dependency.to.cardinality)}${dependency.to.name}${getLabel(dependency.label)}").mkString("\n")
  }

  private val accessModifier: Map[Access, String] = Map(
    Access._public -> "+",
    Access._private -> "-",
    Access._protected -> "#",
    Access._packagePrivate -> "~"
  )

  private val nonAccessModifier: Map[NonAccessModifier, String] = Map(
    NonAccessModifier.STATIC -> " {static}",
    NonAccessModifier.ABSTRACT -> " {abstract}",
  )

  private val dependencyTypes: Map[DependencyType, String] = Map(
    DependencyType.AGGREGATION -> "o--",
    DependencyType.COMPOSITION -> "*--",
    DependencyType.EXTENSION -> "<|--",
  )
}