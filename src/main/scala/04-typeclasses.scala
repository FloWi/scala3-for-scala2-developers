/**
 * TYPECLASSES
 * 
 * Scala 3 introduces direct support for typeclasses using contextual features of the language.
 * Typeclasses provide a way to abstract over similar data types, without having to change the 
 * inheritance hierarchy of those data types, providing the power of "mixin" interfaces, but 
 * with additional flexibility that plays well with third-party data types.
 */
object type_classes:
  trait PrettyPrint[-A]:
    extension (a: A) def prettyPrint: String

  given PrettyPrint[String]:
    extension (a: String) def prettyPrint: String = a

  "foo".prettyPrint

  //this is a way to model higher-kinded prettyPrints
  trait PrettyPrintF[-F[+_]]:
    extension [A] (fa: F[A]) def prettyPrint(using PrettyPrint[A]): String

  trait Derive[F[_], TC[_]]:
    def derive[A](using TC[A]): TC[F[A]]

  // alternative, but covariance annotation doesn't work  
  // type PrettyPrintFAlternative[F[_]] = Derive[F, PrettyPrint]

  final case class Person(name: String, age: Int)

  /**
   * EXERCISE 1
   * 
   * With the help of the `given` keyword, create an instance of the `PrettyPrint` typeclass for the 
   * data type `Person` that renders the person in a pretty way.
   */

  given PrettyPrint[Person]:
    extension (a: Person) def prettyPrint: String = s"Person(name=${a.name}, age: ${a.age})"

  /**
   * EXERCISE 2
   * 
   * With the help of the `given` keyword, create a **named* instance of the `PrettyPrint` typeclass 
   * for the data type `Int` that renders the integer in a pretty way.
   */
  given myIntPrettyPrint as PrettyPrint[Int]:
    extension (a: Int) def prettyPrint: String = a.toString
  
  /**
   * EXERCISE 3
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `String`.
   */
  val stringPrettyPrint: PrettyPrint[String] = summon[PrettyPrint[String]]
  /**
   * EXERCISE 4
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `Int`.
   */
  val intPrettyPrint: PrettyPrint[Int] = summon[PrettyPrint[Int]]

  /**
   * EXERCISE 5
   * 
   * With the help of the `using` keyword, create a method called `prettyPrintIt` that, for any type 
   * `A` for which a `PrettyPrint` instance exists, can both generate a pretty-print string, and 
   * print it out to the console using `println`.
   */
  def prettyPrintIt[A: PrettyPrint](a: A) = println(a.prettyPrint)

  /**
   * EXERCISE 6
   * 
   * With the help of both `given` and `using`, create an instance of the `PrettyPrint` type class
   * for a generic `List[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given [A](using PrettyPrint[A]) as PrettyPrint[List[A]]:
    extension (a: List[A]) def prettyPrint: String = a.map(_.prettyPrint).mkString("\n")

  /**
   * EXERCISE 7
   * 
   * With the help of both `given` and `using`, create a **named** instance of the `PrettyPrint` 
   * type class for a generic `Vector[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  // given vectorPrettyPrint[A] as ...
  given vectorPrettyPrint[A](using p: PrettyPrint[A]) as PrettyPrint[Vector[A]]:
      extension (a: Vector[A]) def prettyPrint: String = a.map(_.prettyPrint).mkString(", ")

  // JdG suggests to avoid naming them
  // good habit to only have one type-class-instance for any given combination of types
  // avoids having a wrong import break your code by unintentially importing some implicits
  // givens need to be imported explicitly e.g. import Identified. { given Identified[UUID] }



  import scala.CanEqual._ 

  /**
   * EXERCISE 8
   * 
   * Using the `derives` clause, derive an instance of the type class `CanEqual` for 
   * `Color`.
   */
  enum Color derives CanEqual: // can be extended like this: enum Color derives CanEqual, Show, Ordering:
    case Red 
    case Green 
    case Blue

/**
 * IMPLICIT CONVERSIONS
 * 
 * Scala 3 introduces a new type class called `Conversion` to perform "implicit 
 * conversions"--the act of automatically converting one type to another.
 */
object conversions:
  final case class Rational(n: Int, d: Int)

  /**
   * EXERCISE 1
   * 
   * Create an instance of the type class `Conversion` for the combination of types
   * `Rational` (from) and `Double` (to).
   */
  // given ...
  given Conversion[Rational, Double] = {
    case Rational(a, b) => a / b
  }

  /**
   * EXERCISE 2
   * 
   * Multiply a rational number by 2.0 (a double) to verify your automatic
   * conversion works as intended.
   */
  Rational(1, 2)
