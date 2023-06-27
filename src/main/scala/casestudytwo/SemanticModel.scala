package casestudytwo

import scala.collection.mutable

case class Entity(name: String, fields: List[Field],
                  methods: List[Method], constructors: List[Constructor],
                  isAbstract: Boolean,
                  isInterface: Boolean)


case class Field(access: Access, name: String, `type`: String, nonAccessModifier: NonAccessModifier)

case class Method(access: Access, name: String, `type`: String,
                  parameters: List[Parameter], nonAccessModifier: NonAccessModifier)

case class Constructor(access: Access, parameters: List[Parameter])

case class Parameter(name: String, `type`: String)

enum Access:
  case _public, _private, _protected, _packagePrivate


case class Enumeration(name: String, values: List[String])

enum DependencyType:
  case EXTENSION, COMPOSITION, AGGREGATION

enum NonAccessModifier:
  case ABSTRACT, STATIC

case class DependencyConcept(name: String, cardinality: String)

case class Dependency(from: DependencyConcept, to: DependencyConcept, dependencyType: DependencyType, label: String)


case class PlantUml(classes: List[Entity], abstractClasses: List[Entity], enums: List[Enumeration],
                    interfaces: List[Entity], dependencies: List[Dependency])
