package compile;

/**
  * Classes to make scala a little more functional
  */
object FunctionalUtils {
  /**
    * An enhanced Either type that implements map and flatMap
    */
  class EnhancedEither[E,A](e: Either[E,A]) {

    /**
      * Applies a function if the Either is a Right
      * Otherwise it propagates the Left
      */
    def map[B](f: A => B) : Either[E,B] = e match {
      case Left(e) => Left(e)
      case Right(x) => Right(f(x))
    }

    /**
      * Applies a function if the Either is a Right
      * Otherwise it propagates the Left
      */
    def flatMap[B](f: A => Either[E,B]) : Either[E,B] = e match {
      case Left(e) => Left(e)
      case Right(x) => f(x)
    }
  }

  /**
    * Automagically adds new methods to Either
    */
  implicit def enhanceEither[E,A](e: Either[E,A]) = new EnhancedEither(e)
}
