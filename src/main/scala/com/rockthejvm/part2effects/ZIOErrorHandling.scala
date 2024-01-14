package com.rockthejvm.part2effects

import zio.*

import java.io.IOException
import java.net.NoRouteToHostException
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

  /*
   * Errors = failures present in the ZIO type signature ("checked" errors)
   * Defects = failures that are unrecoverable, unforeseen, NOT present in the ZIO type signature
   *
   * ZIO[R,E,A] can finish with Exit[E,A]
   * - Success[A] containing A
   * - Cause[E]
   *   - Fail[E] containing the error
   *   - Die(t: Throwable) which was unforeseen
   */
  val divisionByZero: UIO[Int] = ZIO.succeed(1 / 0)

  val failedInt: ZIO[Any, String, Int]                  = ZIO.fail("I failed!")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox
  val failureCauseHidden: ZIO[Any, String, Int]         = failureCauseExposed.unsandbox
  // fold with cause
  val foldedWithCause = failedInt.foldCause(
    cause => s"this failed with ${cause.defects}",
    value => s"this succeeded with $value"
  )
  val foldedWithCause_v2 = failedInt.foldCauseZIO(
    cause => ZIO.succeed(s"this failed with ${cause.defects}"),
    value => ZIO.succeed(s"this succeeded with $value")
  )

  /*
   * Good practice:
    - at a lower level, your "errors" should be treated
    - at a higher level, you should hide "errors" and assume they are unrecoverable
   */
  def callHTTPEndpoint(url: String): ZIO[Any, IOException, String] =
    ZIO.fail(new IOException("no internet, dummy!"))

  val endpointCallWithDefects: ZIO[Any, Nothing, String] =
    callHTTPEndpoint("rockthejvm.com").orDie // all errors are now defects

      // refining the error channel
  def callHTTPEndpointWideError(url: String): ZIO[Any, Exception, String] =
    ZIO.fail(new IOException("No internet!!!"))

  def callHTTPEndpoint_v2(url: String): ZIO[Any, IOException, String] =
    callHTTPEndpointWideError(url).refineOrDie[IOException] {
      case e: IOException            => e
      case _: NoRouteToHostException => new IOException(s"No route to host to $url, can't find page")
    }

  // reverse: turn defects into error channel
  val endpointCallWithError = endpointCallWithDefects.unrefine { case e =>
    e.getMessage
  }

  /*
   * Combine effects with different errors
   */
  case class IndexError(message: String)
  case class DbError(message: String)
  val callApi: ZIO[Any, IndexError, String] = ZIO.succeed("page: <html></html>")
  val queryDb: ZIO[Any, DbError, Int]       = ZIO.succeed(1)
  val combined: ZIO[Any, IndexError | DbError, (String, Int)] = for {
    page         <- callApi
    rowsAffected <- queryDb
  } yield (page, rowsAffected)

  /*
   * Solutions:
    - design an error model (using traits)
    - use Scala 3 union types
    - .mapError to some common error type
   */

  /*
   * Exercises
   */
  // 1 - an effect that fails. Make this effect fail with a TYPED error
  val aBadFailure = ZIO.succeed[Int](throw new RuntimeException("This is bad!"))
  val aGoodFailure = aBadFailure.foldCauseZIO(
    cause => ZIO.fail(cause.defects),
    value => ZIO.succeed(value)
  )
  val aBetterFailure    = aBadFailure.sandbox
  val aBetterFailure_v2 = aBadFailure.unrefine { case e => e }

  // 2 - transform a zio into another zio with a narrower exception type
  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
    zio.refineOrDie { case ioe: IOException => ioe }

  // 3
  def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] =
    zio.foldZIO(
      error => ZIO.fail(Left(error)),
      value =>
        value match {
          case Left(error)  => ZIO.fail(Right(error))
          case Right(value) => ZIO.succeed(value)
        }
    )

  // 4
  val database = Map(
    "daniel" -> 123,
    "alice"  -> 789
  )
  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookupProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if (userId != userId.toLowerCase())
      ZIO.fail(QueryError("User ID format is invalid"))
    else ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))

  // surface out all the failed cases of this API
  def betterLookupProfile(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookupProfile(userId).foldZIO(
      error => ZIO.fail(Some(error)),
      maybeProfile =>
        maybeProfile match
          case None          => ZIO.fail(None)
          case Some(profile) => ZIO.succeed(profile)
    )

  def betterLookupProfile_v2(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookupProfile(userId).some

  override def run = {
    ZIO.succeed(println("hello"))
  }
}
