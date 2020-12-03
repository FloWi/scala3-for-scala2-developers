/**
 * EXTENSION METHODS
 * 
 * Scala 3 brings first-class support for "extension methods", which allow adding methods to 
 * classes after their definition. Previously, this feature was emulated using implicits.
 */
object ext_methods:


//example with futures
  //this might be useless, since Future#zip already exists.
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  extension [A, B](self: Future[A]) def zip(r: Future[B]): Future[(A, B)] =
    self.flatMap(l => r.map(r => (l, r)))



  // no heap allocation when using extension methods
  // scala doesn't look for extensionmethods everywhere. You have to import them
    
  final case class Email(value: String)

  /**
   * EXERCISE 1
   * 
   * Add an extension method to `Email` to retrieve the username of the email address (the part 
   * of the string before the `@` symbol).
   */
  extension (e: Email) def username: String = e.value.takeWhile(_ != '@')

  val sherlock = Email("sherlock@holmes.com").username

  /**
   * EXERCISE 2
   * 
   * Add an extension method to `Email` to retrieve the server of the email address (the part of 
   * the string after the `@` symbol).
   */
  extension (e: Email) def server: String = e.value.dropWhile(_ != '@').tail

  /**
   * EXERCISE 3
   * 
   * Add an extension method to `Option[A]` that can zip one option with another `Option[B]`, to 
   * return an `Option[(A, B)]`.
   */
  extension [A, B] (self: Option[A]) def zip(v: Option[B]): Option[(A, B)] = 
    (self, v) match
      case (Some(a), Some(b)) => Some((a, b))
      case _                  => None 
      

  /**
   * A rational number is one in the form n/m, where n and m are integers.
   */
  final case class Rational(numerator: BigInt, denominator: BigInt)

  /**
   * EXERCISE 4
   * 
   * Add a collection of extension methods to `Rational`, including `+`, to add two rational 
   * numbers, `*`, to multiply two rational numbers, and `-`, to subtract one rational number 
   * from another rational number.
   */
  object RationalExtensions:    
    
    extension (self: Rational):
    
      def * (that: Rational): Rational = Rational(self.numerator * that.numerator, self.denominator * that.denominator)
      def + (that: Rational): Rational = ???
      def - (that: Rational): Rational = ???
  
  object SolutionByAnotherParticipant:
    extension (self: Rational):
      def * (that:Rational): Rational = Rational(that.numerator * self.numerator, self.denominator * that.denominator)
      def + (that: Rational): Rational = Rational(that.denominator * self.numerator + self.denominator * that.numerator , self.denominator * that.numerator)
      def - (that: Rational): Rational = Rational(that.denominator * self.numerator - self.denominator * that.numerator,  self.denominator * that.numerator)

  /**
   * EXERCISE 5
   * 
   * Convert this implicit syntax class to use extension methods.
   */    

  object scope:
    extension (self: String):
      def isSherlock: Boolean = self.startsWith("Sherlock")
      def equalsIgnoreCase(that: String) = self.toLowerCase == that.toLowerCase

  /**
   * EXERCISE 6
   * 
   * Import the extension method `isSherlock` into the following object so the code will compile.
   */
  object test:
    import scope._
    "John Watson".isSherlock
  end test
