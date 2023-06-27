package casestudytwo

import casestudytwo.PlantUmlParser.{constructorParser, fieldParser, methodParser, parameterParser}
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.static
import scala.collection.mutable.ListBuffer

class PlantUmlParserTest extends AnyFunSuite {

  // _________________ Constructor  _________________
  test("test constructor without parameters") {
    // GIVEN
    val constructor = "+ ClassName() <<Constructor>>"

    // WHEN
    val result: PlantUmlParser.ParseResult[Constructor] = PlantUmlParser.parse(constructorParser, constructor)

    // THEN
    assert(result.get.access == Access._public)
    assert(result.get.parameters.isEmpty)
  }

  test("test constructor with parameters") {
    // GIVEN
    val constructor = "+ ClassName(firstParam: String, secondParam: int) <<Constructor>>"

    // WHEN
    val result: PlantUmlParser.ParseResult[Constructor] = PlantUmlParser.parse(constructorParser, constructor)

    // THEN
    assert(result.get.access == Access._public)
    assert(result.get.parameters.size == 2)
    assert(result.get.parameters.head.name == "firstParam")
    assert(result.get.parameters.head.`type` == "String")
    assert(result.get.parameters.last.name == "secondParam")
    assert(result.get.parameters.last.`type` == "int")
  }

  // _________________ Parameter  _________________
  test("test parameter with parameters") {
    // GIVEN
    val parameter = "firstParam: String"

    // WHEN
    val result: PlantUmlParser.ParseResult[Parameter] = PlantUmlParser.parse(parameterParser, parameter)

    // THEN
    assert(result.get.name == "firstParam")
    assert(result.get.`type` == "String")
  }

  // _________________ Methods  _________________
  test("test method without parameter") {
    // GIVEN
    val method = "- testMethod(): Unit"

    // WHEN
    val result: PlantUmlParser.ParseResult[Method] = PlantUmlParser.parse(methodParser, method)

    // THEN
    assert(result.get.access == Access._private)
    assert(result.get.name == "testMethod")
    assert(result.get.parameters.isEmpty)
    assert(result.get.`type` == "Unit")
    assert(result.get.nonAccessModifier == null)
  }

  test("test method with parameters") {
    // GIVEN
    val method = "+ testMethod(firstParameter: Double, secondParameter: int): Unit"

    // WHEN
    val result: PlantUmlParser.ParseResult[Method] = PlantUmlParser.parse(methodParser, method)

    // THEN
    assert(result.get.access == Access._public)
    assert(result.get.name == "testMethod")
    assert(result.get.parameters.size == 2)
    assert(result.get.parameters.head.name == "firstParameter")
    assert(result.get.parameters.head.`type` == "Double")
    assert(result.get.parameters.last.name == "secondParameter")
    assert(result.get.parameters.last.`type` == "int")
    assert(result.get.`type` == "Unit")
  }

  // _________________ Fields  _________________
  test("test field without non access modifier") {
    // GIVEN
    val field = "# testField: String"

    // WHEN
    val result: PlantUmlParser.ParseResult[Field] = PlantUmlParser.parse(fieldParser, field)

    // THEN
    assert(result.get.access == Access._protected)
    assert(result.get.name == "testField")
    assert(result.get.`type` == "String")
    assert(result.get.nonAccessModifier == null)
  }
  test("test field with non access modifier") {
    // GIVEN
    val field = "# testField: String"

    // WHEN
    val result: PlantUmlParser.ParseResult[Field] = PlantUmlParser.parse(fieldParser, field)

    // THEN
    assert(result.get.access == Access._protected)
    assert(result.get.name == "testField")
    assert(result.get.`type` == "String")
    assert(result.get.nonAccessModifier == null)
  }

  // _________________ static and abstract fields and methods  _________________
  test("test static and abstract fields and methods") {
    // GIVEN
    val plantUml: String =
      """@startuml
class MyClass {
+ {static} staticMethod(): void
+ {abstract} abstractMethod(): void
+ {static} staticField: String
+ {abstract} abstractField: String
}
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.classes.size == 1)
    assert(result.classes.head.methods.size == 2)
    assert(result.classes.head.methods.head.nonAccessModifier == NonAccessModifier.STATIC)
    assert(result.classes.head.methods.head.name == "staticMethod")
    assert(result.classes.head.methods.head.`type` == "void")
    assert(result.classes.head.methods.last.nonAccessModifier == NonAccessModifier.ABSTRACT)
    assert(result.classes.head.methods.last.name == "abstractMethod")
    assert(result.classes.head.methods.last.`type` == "void")
    assert(result.classes.head.fields.size == 2)
    assert(result.classes.head.fields.head.nonAccessModifier == NonAccessModifier.STATIC)
    assert(result.classes.head.fields.head.name == "staticField")
    assert(result.classes.head.fields.head.`type` == "String")
    assert(result.classes.head.fields.last.nonAccessModifier == NonAccessModifier.ABSTRACT)
    assert(result.classes.head.fields.last.name == "abstractField")
    assert(result.classes.head.fields.last.`type` == "String")


  }

  // _________________ Class _________________
  test("test class with fields, methods and constructor") {
    // GIVEN
    val plantUml: String =
      """@startuml
class MyClass {
- privateField: String
# protectedField: int
+ publicField: double
~ packageField: boolean
+ MyClass() <<Constructor>>
+ publicMethod(): void
# protectedMethod(): int
- privateMethod(): String
~ packageMethod(): boolean
}
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.classes.size == 1)
    assert(result.classes.head.name == "MyClass")
    assert(result.classes.head.fields.size == 4)
    assert(result.classes.head.fields.head.access == Access._private)
    assert(result.classes.head.fields.head.name == "privateField")
    assert(result.classes.head.fields.head.`type` == "String")
    assert(result.classes.head.fields.last.access == Access._packagePrivate)
    assert(result.classes.head.fields.last.name == "packageField")
    assert(result.classes.head.fields.last.`type` == "boolean")
    assert(result.classes.head.methods.size == 4)
    assert(result.classes.head.methods.head.access == Access._public)
    assert(result.classes.head.methods.head.name == "publicMethod")
    assert(result.classes.head.methods.head.parameters.isEmpty)
    assert(result.classes.head.methods.head.`type` == "void")
    assert(result.classes.head.methods.last.access == Access._packagePrivate)
    assert(result.classes.head.methods.last.name == "packageMethod")
    assert(result.classes.head.methods.last.parameters.isEmpty)
    assert(result.classes.head.methods.last.`type` == "boolean")
    assert(result.classes.head.constructors.size == 1)
    assert(result.classes.head.constructors.head.access == Access._public)
    assert(result.classes.head.constructors.head.parameters.isEmpty)
  }

  test("test class without body") {
    // GIVEN
    val plantUml: String =
      """@startuml
class MyClass
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.classes.size == 1)
    assert(result.classes.head.name == "MyClass")
    assert(result.classes.head.fields.isEmpty)
    assert(result.classes.head.methods.isEmpty)
    assert(result.classes.head.constructors.isEmpty)
    assert(!result.classes.head.isAbstract)
  }

  test("test class body with different order") {
    // GIVEN
    val plantUml: String =
      """@startuml
class MyClass {
+ publicMethod(): void
- privateField: String
}
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.classes.size == 1)
    assert(result.classes.head.methods.size == 1)
    assert(result.classes.head.fields.size == 1)
  }

  // _________________ Abstract class _________________
  test("test abstract class with fields") {
    // GIVEN
    val plantUml: String =
      """@startuml
abstract class MyClass {
- privateField: String
}
      @enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.abstractClasses.last.isAbstract)
    assert(result.abstractClasses.size == 1)
    assert(result.abstractClasses.head.name == "MyClass")
    assert(result.abstractClasses.head.fields.size == 1)
    assert(result.abstractClasses.head.fields.head.access == Access._private)
    assert(result.abstractClasses.head.fields.head.name == "privateField")
    assert(result.abstractClasses.head.fields.head.`type` == "String")

  }

  test("test abstract class without body") {
    // GIVEN
    val plantUml: String =
      """@startuml
abstract class MyClass
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.classes.isEmpty)
    assert(result.abstractClasses.size == 1)
    assert(result.abstractClasses.head.name == "MyClass")
    assert(result.abstractClasses.head.fields.isEmpty)
    assert(result.abstractClasses.head.methods.isEmpty)
    assert(result.abstractClasses.head.constructors.isEmpty)
    assert(result.abstractClasses.head.isAbstract)
  }

  // _________________ Enum _________________
  test("test enum with body") {
    // GIVEN
    val plantUml: String =
      """@startuml
enum TimeUnit {
DAYS
HOURS
MINUTES
}
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.enums.size == 1)
    assert(result.enums.head.name == "TimeUnit")
    assert(result.enums.head.values.size == 3)
    assert(result.enums.head.values.head == "DAYS")
    assert(result.enums.head.values.last == "MINUTES")
  }

  test("test enum without body") {
    // GIVEN
    val plantUml: String =
      """@startuml
enum TimeUnit
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.enums.size == 1)
    assert(result.enums.head.name == "TimeUnit")
    assert(result.enums.head.values.isEmpty)
  }

  // _________________ Interface _________________
  test("test interface with body") {
    // GIVEN
    val plantUml: String =
      """@startuml
interface InterfaceName {
      - privateField: String
+ publicMethod(): void
      }
interface InterfaceName2 {
- privateField2: String
+ publicMethod2(): void
}
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.interfaces.head.isInterface)
    assert(result.interfaces.last.isInterface)
    assert(!result.interfaces.head.isAbstract)
    assert(!result.interfaces.last.isAbstract)
    assert(result.interfaces.size == 2)
    assert(result.interfaces.head.name == "InterfaceName")
    assert(result.interfaces.head.fields.size == 1)
    assert(result.interfaces.head.fields.head.access == Access._private)
    assert(result.interfaces.head.fields.head.name == "privateField")
    assert(result.interfaces.head.fields.head.`type` == "String")
    assert(result.interfaces.head.methods.size == 1)
    assert(result.interfaces.head.methods.head.access == Access._public)
    assert(result.interfaces.head.methods.head.name == "publicMethod")
    assert(result.interfaces.head.methods.head.`type` == "void")
    assert(result.interfaces.head.constructors.isEmpty)
    assert(result.interfaces.last.name == "InterfaceName2")
    assert(result.interfaces.last.fields.size == 1)
    assert(result.interfaces.last.fields.head.access == Access._private)
    assert(result.interfaces.last.fields.head.name == "privateField2")
    assert(result.interfaces.last.fields.head.`type` == "String")
    assert(result.interfaces.last.methods.size == 1)
    assert(result.interfaces.last.methods.head.access == Access._public)
    assert(result.interfaces.last.methods.head.name == "publicMethod2")
    assert(result.interfaces.last.methods.head.`type` == "void")
    assert(result.interfaces.last.constructors.isEmpty)
  }

  test("test interface without body") {
    // GIVEN
    val plantUml: String =
      """@startuml
interface InterfaceName
interface InterfaceName2
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.interfaces.head.isInterface)
    assert(result.interfaces.last.isInterface)
    assert(!result.interfaces.head.isAbstract)
    assert(!result.interfaces.last.isAbstract)
    assert(result.interfaces.size == 2)
    assert(result.interfaces.head.name == "InterfaceName")
    assert(result.interfaces.head.fields.isEmpty)
    assert(result.interfaces.head.methods.isEmpty)
    assert(result.interfaces.last.name == "InterfaceName2")
    assert(result.interfaces.last.fields.isEmpty)
    assert(result.interfaces.last.methods.isEmpty)
  }

  // _________________ Dependency _________________

  test("test dependency between concepts") {
    // GIVEN
    val plantUml: String =
      """@startuml
class Class
class Class2
abstract class AbstractClass
enum Enum
interface Interface
AbstractClass <|-- Class
Class <|-- Class2
Class2 *-- Enum
Class2 *-- Interface
@enduml"""

    // WHEN
    val resultObject: PlantUml = PlantUmlParser(plantUml)
    val resultString = ScalaToPlantUml.mapPlantUmlToPlantUmlString(resultObject)

    // THEN
    assert(resultObject.abstractClasses.size == 1)
    assert(resultObject.abstractClasses.head.name == "AbstractClass")
    assert(resultObject.abstractClasses.head.isAbstract)
    assert(resultObject.classes.size == 2)
    assert(resultObject.classes.head.name == "Class")
    assert(resultObject.classes.last.name == "Class2")
    assert(resultObject.enums.size == 1)
    assert(resultObject.enums.head.name == "Enum")
    assert(resultObject.interfaces.size == 1)
    assert(resultObject.interfaces.head.name == "Interface")
    assert(resultObject.dependencies.size == 4)
    assert(resultObject.dependencies.head.from.name == "AbstractClass")
    assert(resultObject.dependencies.head.to.name == "Class")
    assert(resultObject.dependencies.last.from.name == "Class2")
    assert(resultObject.dependencies.last.to.name == "Interface")

    assert(resultString.trim == plantUml)
  }

  test("test dependency between concepts with labels") {
    // GIVEN
    val plantUml: String =
      """@startuml
class Class
class Class2
enum Enum
Class "1" <|-- "many" Class2
Class2 *-- Enum
@enduml"""

    // WHEN
    val resultObject: PlantUml = PlantUmlParser(plantUml)
    val resultString = ScalaToPlantUml.mapPlantUmlToPlantUmlString(resultObject)

    // THEN
    assert(resultObject.dependencies.head.from.name == "Class")
    assert(resultObject.dependencies.head.to.name == "Class2")
    assert(resultObject.dependencies.head.from.cardinality == "1")
    assert(resultObject.dependencies.head.to.cardinality == "many")

    assert(resultObject.dependencies.last.from.name == "Class2")
    assert(resultObject.dependencies.last.to.name == "Enum")
    assert(resultObject.dependencies.last.from.cardinality == "")
    assert(resultObject.dependencies.last.to.cardinality == "")

    assert(resultString.trim == plantUml)
  }

  test("test dependency between concepts with label") {
    // GIVEN
    val plantUml: String =
      """@startuml
class Class
class Class2
abstract class AbstractClass
AbstractClass <|-- Class : label test
Class "1" <|-- "many" Class2 : test
@enduml"""

    // WHEN
    val resultObject: PlantUml = PlantUmlParser(plantUml)
    val resultString = ScalaToPlantUml.mapPlantUmlToPlantUmlString(resultObject)

    // THEN
    // THEN
    assert(resultObject.dependencies.head.from.name == "AbstractClass")
    assert(resultObject.dependencies.head.to.name == "Class")
    assert(resultObject.dependencies.head.from.cardinality == "")
    assert(resultObject.dependencies.head.to.cardinality == "")

    assert(resultObject.dependencies.last.from.name == "Class")
    assert(resultObject.dependencies.last.to.name == "Class2")
    assert(resultObject.dependencies.last.from.cardinality == "1")
    assert(resultObject.dependencies.last.to.cardinality == "many")

    assert(resultString.trim == plantUml)
  }




  // _________________  With all concepts _________________
  test("test with all concepts multiple times without body") {
    // GIVEN
    val plantUml: String =
      """@startuml
class Class
class Class2
enum Enum
abstract class AbstractClass
interface Interface
abstract class AbstractClass2
enum Enum2
interface Interface2
@enduml"""

    // WHEN
    val result: PlantUml = PlantUmlParser(plantUml)

    // THEN
    assert(result.abstractClasses.size == 2)
    assert(result.abstractClasses.head.name == "AbstractClass")
    assert(result.abstractClasses.head.isAbstract)
    assert(result.abstractClasses.last.name == "AbstractClass2")
    assert(result.abstractClasses.last.isAbstract)
    assert(result.classes.size == 2)
    assert(result.classes.head.name == "Class")
    assert(!result.classes.head.isAbstract)
    assert(result.classes.last.name == "Class2")
    assert(!result.classes.last.isAbstract)
    assert(result.enums.size == 2)
    assert(result.enums.head.name == "Enum")
    assert(result.enums.last.name == "Enum2")
    assert(result.interfaces.size == 2)
    assert(result.interfaces.head.name == "Interface")
    assert(!result.interfaces.head.isAbstract)
    assert(result.interfaces.last.name == "Interface2")
    assert(!result.interfaces.last.isAbstract)

  }

  test("test with all concepts multiple times with body") {
    // GIVEN
    val plantUml: String =
      """@startuml
class Class {
- privateField: String
+ Class() <<Constructor>>
- privateMethod(): String
}
abstract class AbstractClass {
- privateField: String
+ AbstractClass() <<Constructor>>
- privateMethod(): String
}
enum Enum {
DAYS
HOURS
MINUTES
}
interface InterfaceName {
- privateField: String
+ publicMethod(): void
}
AbstractClass <|-- Class
Class <|-- Class2
Class2 *-- Enum
Class2 *-- Interface
@enduml"""

    // WHEN
    val resultObject: PlantUml = PlantUmlParser(plantUml)
    val resultString = ScalaToPlantUml.mapPlantUmlToPlantUmlString(resultObject)


    // THEN
    assert(resultObject.abstractClasses.size == 1)
    assert(resultObject.abstractClasses.head.name == "AbstractClass")
    assert(resultObject.abstractClasses.head.isAbstract)
    assert(resultObject.classes.size == 1)
    assert(resultObject.classes.head.name == "Class")
    assert(!resultObject.classes.head.isAbstract)
    assert(resultObject.enums.size == 1)
    assert(resultObject.enums.head.name == "Enum")
    assert(resultObject.interfaces.size == 1)
    assert(resultObject.interfaces.head.name == "InterfaceName")


    assert(resultObject.abstractClasses.head.fields.size == 1)
    assert(resultObject.abstractClasses.head.fields.head.access == Access._private)
    assert(resultObject.abstractClasses.head.fields.head.name == "privateField")
    assert(resultObject.abstractClasses.head.fields.head.`type` == "String")
    assert(resultObject.abstractClasses.head.constructors.size == 1)
    assert(resultObject.abstractClasses.head.constructors.head.access == Access._public)
    assert(resultObject.abstractClasses.head.constructors.head.parameters.isEmpty)
    assert(resultObject.abstractClasses.head.methods.size == 1)
    assert(resultObject.abstractClasses.head.methods.head.access == Access._private)
    assert(resultObject.abstractClasses.head.methods.head.name == "privateMethod")
    assert(resultObject.abstractClasses.head.methods.head.`type` == "String")
    assert(resultObject.abstractClasses.head.methods.head.parameters.isEmpty)

    assert(resultObject.classes.head.fields.size == 1)
    assert(resultObject.classes.head.fields.head.access == Access._private)
    assert(resultObject.classes.head.fields.head.name == "privateField")
    assert(resultObject.classes.head.fields.head.`type` == "String")
    assert(resultObject.classes.head.constructors.size == 1)
    assert(resultObject.classes.head.constructors.head.access == Access._public)
    assert(resultObject.classes.head.constructors.head.parameters.isEmpty)
    assert(resultObject.classes.head.methods.size == 1)
    assert(resultObject.classes.head.methods.head.access == Access._private)
    assert(resultObject.classes.head.methods.head.name == "privateMethod")
    assert(resultObject.classes.head.methods.head.`type` == "String")
    assert(resultObject.classes.head.methods.head.parameters.isEmpty)

    assert(resultObject.enums.head.values.size == 3)
    assert(resultObject.enums.head.values.head == "DAYS")
    assert(resultObject.enums.head.values.last == "MINUTES")

    assert(resultObject.interfaces.head.fields.size == 1)
    assert(resultObject.interfaces.head.fields.head.access == Access._private)
    assert(resultObject.interfaces.head.fields.head.name == "privateField")
    assert(resultObject.interfaces.head.fields.head.`type` == "String")
    assert(resultObject.interfaces.head.methods.size == 1)
    assert(resultObject.interfaces.head.methods.head.access == Access._public)
    assert(resultObject.interfaces.head.methods.head.name == "publicMethod")
    assert(resultObject.interfaces.head.methods.head.`type` == "void")
    assert(resultObject.interfaces.head.methods.head.parameters.isEmpty)

    assert(resultString.trim == plantUml)
  }

  // _________________  test for loc _________________
  test("test for loc") {
    // GIVEN
    val plantUml: String =
      """@startuml
class ClassName {
- {static} privateField: String
+ ClassName(privateField: String) <<Constructor>>
+ {static} publicMethod(parameter: String): void
}
abstract class AbstractClassName {
# {abstract} protectedField: int
+ AbstractClassName(protectedField: int) <<Constructor>>
- {static} privateMethod(parameter: String): void
}
enum EnumName {
VALUE1
VALUE2
VALUE3
}
interface InterfaceName {
+ publicField: String
+ publicMethod(parameter: String): Map
}
ClassName <|-- EnumName
ClassName "1" *-- "many" AbstractClassName : test label
AbstractClassName o-- "*" ClassName
ClassName o-- AbstractClassName : test
@enduml""".stripMargin

    // WHEN
    val resultObject: PlantUml = PlantUmlParser(plantUml)
    val resultString = ScalaToPlantUml.mapPlantUmlToPlantUmlString(resultObject)

    // THEN
    assert(plantUml == resultString)
  }
}