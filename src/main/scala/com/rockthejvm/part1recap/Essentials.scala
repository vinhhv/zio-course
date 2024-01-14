package com.rockthejvm.part1recap

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Essentials {
  // values
  val aBoolean: Boolean = false

  // expressions are EVALUATED to a value
  val anIfExpression = if (2 > 3) "bigger" else "smaller"

  // instructions vs expressions
  val theUnit = println("Hello, Scala") // Unit = "void" in other languages

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit

    // inheritance model: extend <= 1 class, but inherit from >= 0 traits
    class Crocodile extends Animal with Carnivore {
      override def eat(animal: Animal): Unit = println("Crunch")
    }

    // singleton
    object MySingleton // singleton pattern in one line

    // companions
    object Carnivore // companion object of the class Carnivore

    // generics
    class MyList[A]

    // method notation
    val three        = 1 + 2
    val anotherThree = 1.+(2)

    // functional programming
    val incrementer: Int => Int = x => x + 1
    val incremented             = incrementer(45)

    // map, flatMap, filter
    val processedList = List(1, 2, 3).map(incrementer)
    val aLongerList   = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1, 2, 2, 3, 3, 4)

    // options and try
    val anOption: Option[Int]      = Option(3)
    val doubledOption: Option[Int] = anOption.map(_ * 2)

    val anAttempt                  = Try(42)
    val aModifiedAttempt: Try[Int] = anAttempt.map(_ + 10)

    // pattern matching
    val anUnknown: Any = 45
    val ordinal = anUnknown match {
      case 1 => "first"
      case 2 => "second"
      case _ => "unknown"
    }

    val optionDescription = anOption match {
      case Some(value) => s"the option is not empty: $value"
      case None        => "the option is empty"
    }

    // Futures
    given ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    val aFuture = Future {
      // a bit of code
      42
    }

    // wait for completion (async)
    aFuture.onComplete {
      case Success(value)     => println(s"The async meaning of life is $value")
      case Failure(exception) => println(s"Failed: $exception")
    }

    // partial function
    val aPartialFunction: PartialFunction[Int, Int] = {
      case 1 => 43
      case 8 => 56
    }

    trait HigherKindedType[F[_]]
  }

  def main(args: Array[String]): Unit = {}
}
