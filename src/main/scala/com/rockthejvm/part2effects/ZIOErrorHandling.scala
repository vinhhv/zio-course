package com.rockthejvm.part2effects

import zio.*

import scala.util.{Failure, Success, Try}

object ZIOErrorHandling extends ZIOAppDefault {

  // ZIO can fail
  val aFailedZIO            = ZIO.fail("Something went wrong")
  val failedWithThrowable   = ZIO.fail(new RuntimeException("Boom!"))
  val failedWithDescription = failedWithThrowable.mapError(_.getMessage)

  // attempt: run an effect that might throw an exception
  val badZIO = ZIO.succeed {
    println("Trying something")
    val string: String = null
    string.length()
  } // this is bad

  // use attempt if you're ever unsure whether your code might throw
  val anAttempt: ZIO[Any, Throwable, Int] = ZIO.attempt {
    println("Trying something")
    val string: String = null
    string.length()
  }

  // effectfully catch errors
  val catchError = anAttempt.catchAll(e => ZIO.succeed(s"Returning a different value because $e"))
  val catchSelectedErrors = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime exceptions: $e")
    case _                   => ZIO.succeed("Ignoring everything else")
  }

  // chain effects
  val aBetterAttempt = anAttempt.orElse(ZIO.succeed(56))
  // fold: handle both success and failure
  val handleBoth: ZIO[Any, Nothing, String] =
    anAttempt.fold(ex => s"Something bad happened: $ex", value => s"Length of the string was $value")
  // effectful fold: foldZIO
  val handleBoth_v2 = anAttempt.foldZIO(
    ex => ZIO.succeed(s"Something bad happened: $ex"),
    value => ZIO.succeed(s"Length of the string was $value")
  )

  /*
    Conversions between Option/Try/Either to ZIO
   */
  val aTryToZIO: ZIO[Any, Throwable, Int] = ZIO.fromTry(Try(42 / 0)) // can fail with Throwable

  // either -> ZIO
  val anEither: Either[Int, String]        = Right("Success")
  val anEitherToZIO: ZIO[Any, Int, String] = ZIO.fromEither(anEither)

  // ZIO -> ZIO with Either as the value channel
  val eitherZIO = anAttempt.either
  // reverse
  val anAttempt_v2 = eitherZIO.absolve

  // option -> ZIO
  val anOption: ZIO[Any, Option[Nothing], Int] = ZIO.fromOption(Some(42))

  /*
   * Exercise: implement a version of fromTry, fromOption, fromEither, either, absolve
   * using fold and foldZIO
   */
  def fromTry[A](_try: Try[A]): Task[A] =
    _try.fold(
      throwable => ZIO.fail(throwable),
      value => ZIO.succeed(value)
    )

  def fromOption[A](option: Option[A]): IO[Option[Nothing], A] =
    option.fold(ZIO.fail(None))(value => ZIO.succeed(value))

  def fromEither[E, A](either: Either[E, A]): IO[E, A] =
    either.fold(error => ZIO.fail(error), value => ZIO.succeed(value))

  def either[R, E, A](zio: ZIO[R, E, A]): URIO[R, Either[E, A]] =
    zio.foldZIO(
      e => ZIO.succeed(Left(e)),
      v => ZIO.succeed(Right(v))
    )

  def absolve[R, E1, E2 >: E1, A](zio: ZIO[R, E1, Either[E2, A]]): ZIO[R, E2, A] =
    zio.foldZIO(
      e => ZIO.fail(e),
      v =>
        v match
          case Left(value)  => ZIO.fail(value)
          case Right(value) => ZIO.succeed(value)
    )

  def absolve_v2[R, E, A](zio: ZIO[R, Nothing, Either[E, A]]): ZIO[R, E, A] =
    zio.flatMap {
      case Left(error)  => ZIO.fail(error)
      case Right(value) => ZIO.succeed(value)
    }

  override def run = {
    ZIO.succeed(println("hello"))
  }
}
