package com.rockthejvm.part3concurrency

import zio.*

import java.io.{File, FileReader, FileWriter}
import com.rockthejvm.utils.*

object Fibers extends ZIOAppDefault {

  val meaningOfLife = ZIO.succeed(42)
  val favLang       = ZIO.succeed("Scala")

  // Fiber = lightweight thread
  //       = description of a computation that will be performed by one of the threads on the ZIO runtime
  //       similar to how regular threads are a description of a computation of the main thread of the JVM
  def createFiber: Fiber[Throwable, String] = ??? // impossible to create manually

  val sameThreadIO = for {
    mol  <- meaningOfLife.debugThread
    lang <- favLang.debugThread
  } yield (mol, lang)

  val differentThreadIO = for {
    _ <- meaningOfLife.debugThread.fork
    _ <- favLang.debugThread.fork
  } yield ()

  val meaningOfLifeFiber: ZIO[Any, Nothing, Fiber[Throwable, Int]] = meaningOfLife.fork

  // join a fiber
  def runOnAnotherThread[R, E, A](zio: ZIO[R, E, A]) = for {
    fib    <- zio.fork
    result <- fib.join
  } yield result

  // awaiting a fiber => same as join, but exposes its Exit datastructure
  def runOnAnotherThread_v2[R, E, A](zio: ZIO[R, E, A]) = for {
    fib    <- zio.fork
    result <- fib.await
  } yield result match {
    case Exit.Success(value) => s"Succeeded with $value"
    case Exit.Failure(cause) => s"Failed with $cause"
  }

  // poll - peek at result of the fiber RIGHT NOW, without blocking
  val peekFiber = for {
    fib <- ZIO.attempt {
      Thread.sleep(1000)
      42
    }.fork
    result <- fib.poll
  } yield result

  // compose fibers

  // zip
  val zippedFibers = for {
    fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber = fib1.zip(fib2)
    tuple <- fiber.join
  } yield tuple

  // orElse
  val chainedFibers = for {
    fiber1 <- ZIO.fail("not good!").debugThread.fork
    fiber2 <- ZIO.succeed("Rock the JVM!").debugThread.fork
    fiber = fiber1.orElse(fiber2)
    message <- fiber.join
  } yield message

  /**
   * Exercises
   */
  // 1 - zip two fibers without using the zip combinator
  // hint: create a fiber that waits for both
  def zipFibers[E, A1, A2](fiber1: Fiber[E, A1], fiber2: Fiber[E, A2]): ZIO[Any, Nothing, Fiber[E, (A1, A2)]] = {
    // fiber1.mapFiber(a1 => fiber2.map(a2 => (a1, a2)))
    (
      for {
        v1 <- fiber1.join
        v2 <- fiber2.join
      } yield (v1, v2)
    ).fork
  }

  val zippedFibers_v2 = for {
    fib1  <- ZIO.succeed("Result from fiber 1").debugThread.fork
    fib2  <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber <- zipFibers(fib1, fib2)
    tuple <- fiber.join
  } yield tuple

  def zipFibersGeneral[E, E1 <: E, E2 <: E, A1, A2](
      fiber1: Fiber[E1, A1],
      fiber2: Fiber[E2, A2]
  ): ZIO[Any, Nothing, Fiber[E, (A1, A2)]] = {
    // fiber1.mapFiber(a1 => fiber2.map(a2 => (a1, a2)))
    (
      for {
        v1 <- fiber1.join
        v2 <- fiber2.join
      } yield (v1, v2)
    ).fork
  }

  // 2 - same thing with orElse
  def chainFibers[E, A](fiber1: Fiber[E, A], fiber2: Fiber[E, A]): ZIO[Any, Nothing, Fiber[E, A]] = {
    fiber1.join.orElse(fiber2.join).fork
    // val waitFiber1  = fiber1.join
    // val waitFiber2  = fiber2.join
    // val finalEffect = waitFiber1.orElse(waitFiber2)
    // finalEffect.fork
  }

  // 3 - distributing tasks in between many fibers
  // spawn n fibers, count the n of words in each file,
  // then aggregate all the results together in one big manner
  def generateRandomFile(path: String): Unit = {
    val random = scala.util.Random
    val chars  = 'a' to 'z'
    val nWords = random.nextInt(2000) // at most 2000 words

    val content = (1 to nWords)
      .map(_ =>
        (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString
      ) // one word for every 1 to nWords
      .mkString(" ")

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()
  }

  // part1 - an effect which reads on file and counts the words there
  def countWords(path: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split(" ").count(_.nonEmpty)
      println(s"Counted $nWords in $path")
      source.close()
      nWords
    }

  def testFileName(i: Int): String =
    s"src/main/resources/testfile_$i.txt"

  // part 2 - spin up fibers for all the files
  def wordCountParallel(n: Int): UIO[Int] = {
    val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to n)
      .map(testFileName)       // paths
      .map(countWords)         // list of effects
      .map(_.debugThread.fork) // list of effects returning fibers
      .map((fiberEffect: ZIO[Any, Nothing, Fiber[Nothing, Int]]) =>
        fiberEffect.flatMap(_.join)
      ) // list of effects returning values (count of words)

    effects.reduce { (zioa, ziob) =>
      for {
        a <- zioa
        b <- ziob
      } yield a + b
    }
  }

  def run = {
    val fileCount = 10
    // ZIO
    //   .succeed(
    //     (1 to fileCount)
    //       .foreach(i => generateRandomFile(testFileName(i)))
    //   ) *>
    wordCountParallel(fileCount).debugThread
  }
}
