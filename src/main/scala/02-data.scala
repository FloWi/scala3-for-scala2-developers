/**
 * ENUMS
 * 
 * Scala 3 adds support for "enums", which are to sealed traits like case classes 
 * were to classes. That is, enums cut down on the boilerplate required to use 
 * the "sealed trait" pattern for modeling so-called sum types, in a fashion very 
 * similar to how case classes cut down on the boilerplate required to use 
 * classes to model so-called product types.
 * 
 * Strictly speaking, Scala 3 enums are not the same as Java enums: while the 
 * constructors of enums are finite, and defined statically at compile-time in the 
 * same file, these constructors may have parameters, and therefore, the total 
 * number of values of any enum type could be large or infinite.
 * 
 * Enums and case classes provide first-class support for "algebraic data types" 
 * in Scala 3.
 */
package enums: 
  /**
   * EXERCISE 1
   * 
   * Convert this "sealed trait" to an enum.
   */
  enum DayOfWeek:  
    case Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday 

  //It forces you to comply to scala2's convention of putting the values into the companion object
  /**
   * EXERCISE 2
   * 
   * Explore interop with Java enums by finding all values of `DayOfWeek`, and by 
   * finding the value corresponding to the string "Sunday".
   */
  def daysOfWeek: Array[DayOfWeek] = DayOfWeek.values //yay - java method. But this only works if none of the enum-cases has a parameter
  def sunday: DayOfWeek = DayOfWeek.valueOf("Sunday") //will blow up at runtime though

  /**
   * EXERCISE 3
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type of any of the case constructors!
   */
  enum Color:
    case Red
    case Green
    case Blue
    case Custom(red: Int, green: Int, blue: Int)

  //yay, this is of tpe Color now. With sealed trait encoding it'd be inferred as type Color.Custom and we were able to upcast it.
  //you can still achieve the scala2 behavior like this
  // val custom: Color.Custom = Color.Custom(12,123,123)
  // val custom: Color.Custom = new Color.Custom(12,123,123) //this will automatically infer Color.Custom
  val custom: Color = Color.Custom(12,123,123) 

  val foo = List(1,2,3).foldLeft(Option.empty[Int]) {
    case (None, i) if i == 2 => Some(i)
    case (opt, _)            => opt
  }
  // this is very helpful in e.g. in folds. in the example above we have to write 
  // .foldLeft(Option.empty[Int]) and can't use .foldLeft(None), because Option is (still) not modelled as an enum
  
  //this works just fine :)
  val bar = List(1,2,3).foldLeft(Color.Red) {
    case (Color.Red, i) if i == 2 => Color.Custom(i,i,i)
    case (opt, _)            => opt
  }

  // so it's time for us to introduce or own Option-type :)
  /*
  enum Maybe[+T]:
    case None
    case Some(v: T)

  val x = List(1,2,3).foldLeft(Maybe.None) {
    case (Maybe.None, i) if i == 2 => Maybe.Some(i)
    case (opt, _)            => opt
  }
  //wasn't able to get it to compile
  */

  /**
   * EXERCISE 4
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type parameters in the case constructors!
   */
  enum Result[+Error, +Value]:
    case Succeed(value: Value)
    case Fail(error: Error)

  /**
   * EXERCISE 5
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type parameters in the case constructors!
   */
  enum Workflow[-Input, +Output]:
    case End(value: Output)

  /**
   * EXERCISE 6
   * 
   * Convert this "sealed trait" to an enum.
   */
  enum Conversion[-From, +To]:  
    case AnyToString extends Conversion[Any, String]
    case StringToInt extends Conversion[String, Option[Int]]

    //defining GADT's is a bit more verbose, but scalac cannot guess what you want :)
  val c = Conversion.AnyToString


/**
 * CASE CLASSES
 * 
 * Scala 3 makes a number of improvements to case classes.
 */
package case_classes:
  /**
   * EXERCISE 1
   * 
   * By making the public constructor private, make a smart constructor for `Email` so that only 
   * valid emails may be created.
   */
  final case class Email private (value: String)
  object Email:
    def fromString(v: String): Option[Email] = 
      if isValidEmail(v) then Some(Email(v)) else None

    def isValidEmail(v: String): Boolean = v.matches("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$")
  

  /**
   * EXERCISE 2
   * 
   * Try to make a copy of an existing `Email` using `Email#copy` and note what happens.
   * 
   */
  def changeEmail(email: Email): Email = ??? //email.copy(email = "not a real email!") //yay - the private keyword also makes the copy-method private :)

  /**
   * EXERCISE 3
   * 
   * Try to create an Email directly by using the generated constructor in the companion object.
   * 
   */
  def caseClassApply(value: String): Email = ??? //Email("broken") //now this doesn't compile either, since the apply method is also private

/**
 * PATTERN MATCHING
 * 
 * Scala 3 provides upgrades to the power and flexibility of pattern matching.
 */  
object pattern_matching:
  /**
   */
  def foo: Int = 2
