package casestudytwo

import casestudytwo.ScalaToPlantUml.mapPlantUmlToPlantUmlString

object SemanticModelBuilder {
  def getFields(bodyConcepts: List[Object]): List[Field] = bodyConcepts.collect { case field: Field => field }

  def getMethods(bodyConcepts: List[Object]): List[Method] = bodyConcepts.collect { case method: Method => method }

  def getConstructors(bodyConcepts: List[Object]): List[Constructor] =
    bodyConcepts.collect { case constructor: Constructor => constructor }

  def buildPlantUml(concepts: List[Object]): PlantUml = {
    val classes = concepts.collect { case clazz: Entity if !clazz.isAbstract && !clazz.isInterface => clazz }
    val abstractClasses = concepts.collect { case clazz: Entity if clazz.isAbstract => clazz }
    val enums = concepts.collect { case enumeration: Enumeration => enumeration }
    val interfaces = concepts.collect { case clazz: Entity if clazz.isInterface => clazz }
    val dependencies = concepts.collect { case dependency: Dependency => dependency }
    PlantUml(classes, abstractClasses, enums, interfaces, dependencies)
  }

  private def validateDependencies(uml: PlantUml): PlantUml = {
    uml.dependencies.find(dependency => !uml.classes.exists(clazz => clazz.name == dependency.from.name) ||
      !uml.enums.exists(enumaration => enumaration.name == dependency.from.name) ||
      !uml.abstractClasses.exists(abstractClass => abstractClass.name == dependency.from.name) ||
      !uml.interfaces.exists(interface => interface.name == dependency.from.name)
    ) match
      case Some(dependency) => throw new Exception(s"Class ${dependency.from.name} does not exist")
      case None => ()
    uml
  }
}