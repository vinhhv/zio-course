package com.rockthejvm.part2effects

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.Try

object Effects {

  // functional programming
  // EXPRESSIONS
  def combine(a: Int, b: Int): Int = a + b

  // local reasoning = type signature describes the kind of computation that will be performed
  // referential transparency = ability to replace an expression with the value that it evaluates to
  val five    = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // not all expressions are RT
  val resultOfPrinting: Unit    = println("Learning ZIO")
  val resultOfPrinting_v2: Unit = () // not the same

  // example 2: changing a variable
  var anInt                = 0
  val changingInt: Unit    = (anInt = 42) // side effect
  val changingInt_v2: Unit = ()           // not the same program

  // side effects are inevitable
  /*
   * Effect properties:
   * - the type signature describes what KIND of computation it will perform
   * - the type signature describes the type of VALUE that it will produce
   * - if side effects are required, construction must be separate from the EXECUTION
   */

  /*
   * Example: Option = possibly absent values
   * - type signature describes the kind of computation = a possibly absent value
   * - type signature says that the computation returns an A, if the computation does produce something
   * - no side effects are needed
   *
   * => Option is an effect
   */
  val anOption: Option[Int] = Option(42)

  /*
   * Example 2: Future
   * - describes an asynchronous computation
   * - produces a value of type A, if it finishes and it's successful
   * - side effects are required, CONSTRUCTION is NOT SEPARATE from execution
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
   * Example 3: MyIO
   * - describes a computation which might perform side effects
   * - produces values of type A if the computation is successful
   * - side effects are required, construction IS SEPARATE from execution
   *
   * => MyIO is an effect
   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIOWithSideEffects = MyIO(() => {
    println("producing side effect")
    42
  })

  /*
   * Exercises - create some IO
   * 1. measure the current time of the system
   * 2. measure the duration of a function
   * - use exercise 1
   * - use map/flatMap combinations of MyIO
   * 3. read something from the console
   * 4. print something to the console (e.g. "what's your name"), then read, then print a welcome message
   */

  // 1
  def getCurrentTime(): MyIO[Long] = MyIO(() => {
    System.currentTimeMillis()
  })
  // 2
  def measure[A](computation: MyIO[A]): MyIO[(Long, A)] =
    for {
      start <- getCurrentTime()
      value <- computation
      end   <- getCurrentTime()
    } yield (end - start, value)
  // 3
  def readConsole(): MyIO[String] = MyIO(() => {
    StdIn.readLine()
  })
  // 4
  def welcome(): MyIO[Unit] =
    for {
      _    <- MyIO(() => println("What's your name?"))
      name <- readConsole()
    } yield println(s"Welcome $name!")

  /*
   * A simplified ZIO
   */
  case class MyZIO[-R, +E, +A](unsafeRun: R => Either[E, A]) {
    def map[B](f: A => B): MyZIO[R, E, B] =
      MyZIO(r =>
        unsafeRun(r) match {
          case Left(e)  => Left(e)
          case Right(v) => Right(f(v))
        }
      )

    def flatMap[R1 <: R, E1 >: E, B](f: A => MyZIO[R1, E1, B]): MyZIO[R1, E1, B] =
      MyZIO(r =>
        unsafeRun(r) match {
          case Left(e)  => Left(e)
          case Right(v) => f(v).unsafeRun(r)
        }
      )
  }

  def main(args: Array[String]): Unit = {
    anIOWithSideEffects.unsafeRun()
    val (time, value) = measure(MyIO(() => {
      Thread.sleep(1000)
      1000
    })).unsafeRun()
    println(s"Time = $time\nvalue = $value")
    welcome().unsafeRun()
  }
}
