package com.rockthejvm.part2effects

import zio.*

import scala.io.StdIn

object ZIOEffects {
  // success
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  // failure
  val aFailure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")
  // suspension/delay
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  // map + flatMap
  val improvedMOL = meaningOfLife.map(_ * 2)
  val printingMOL = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))

  // for comprehensions
  val smallProgram = for {
    _    <- ZIO.succeed(println("What's your name?"))
    name <- ZIO.succeed(StdIn.readLine())
    _    <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
  } yield ()

  // A LOT of combinators
  // zip, zipWith
  val anotherMOL  = ZIO.succeed(100)
  val tupledZIO   = meaningOfLife.zip(anotherMOL)
  val combinedZIO = meaningOfLife.zipWith(anotherMOL)(_ * _)

  /*
   * Type aliases of ZIOs
   */

  // UIO[A] = ZIO[Any, Nothing, A] => no requirements, cannot fail produces A
  val aUIO: UIO[Int] = ZIO.succeed(99)
  // URIO[R, A] = ZIO[R, Nothing, A] - cannot fail
  val aURIO: URIO[Int, Int] = ZIO.succeed(67)
  // RIO[R, A] = ZIO[R, Throwable, A] - can fail with a Throwable
  val anRIO: RIO[Int, Int]      = ZIO.succeed(98)
  val aFailedRIO: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))

  // Task[A] = ZIO[Any, Throwable, A] => no requirements, can fail with a Throwable, produces A
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89)
  val aFailedTask: Task[Int]     = ZIO.fail(new RuntimeException("Something bad"))

  // IO[E, A] = ZIO[Any, E, A] - no requirements
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int]     = ZIO.fail("Something bad happened")

  /*
   * Exercises
   */

  // 1 - sequence two ZIOs and take the value of the last one
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      _ <- zioa
      b <- ziob
    } yield b
    // zioa *> ziob

  // 2 - sequence two ZIOs and take the value of the first one
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    for {
      a <- zioa
      _ <- ziob
    } yield a
  // zioa <* ziob

  // 3 - run a ZIO forever
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.flatMap(_ => runForever(zio))
    // zio *> runForever(zio) // allocates on the heap, runs tail-recursively

  lazy val endlessLoop = runForever {
    ZIO.succeed {
      println("running...")
      Thread.sleep(1000)
    }
  }

  // 4 - convert the value of a ZIO to something else
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    zio.map(_ => value)
    // zio.as(value)

  // 5 - discard the value of a ZIO to Unit
  def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    zio.map(_ => ())
    // convert(zio, ())
    // zio.unit

  // 6 - recursion
  def sum(n: Int): Int =
    if (n == 0) 0
    else n + sum(n - 1) // will crash at sum (20000)

  def sumZIO(n: BigInt): UIO[BigInt] = {
    def sumZIOR(_n: BigInt, sum: BigInt): UIO[BigInt] =
      if (_n == 0) ZIO.succeed(sum)
      else sumZIOR(_n - 1, sum + _n)

    sumZIOR(n, 0)
  }

  def sumZIO_v2(n: BigInt): UIO[BigInt] =
    if (n == 0) ZIO.succeed(0)
    else
      for {
        current <- ZIO.succeed(n)
        prevSum <- sumZIO(n - 1)
      } yield current + prevSum

  // 7 - fibonacci
  // hint: use ZIO.suspend/ZIO.suspendSucceed
  def fiboZIO(n: Int): UIO[BigInt] = {
    def fiboZIOR(_n: Int, prev: BigInt, cur: BigInt): UIO[BigInt] =
      if (_n < 2) ZIO.succeed(cur)
      else fiboZIOR(_n - 1, cur, prev + cur)

    if (n == 0) ZIO.succeed(0)
    else if (n == 1) ZIO.succeed(1)
    else fiboZIOR(n, 0, 1)
  }

  def fiboZIO_v2(n: Int): UIO[BigInt] =
    if (n <= 2) ZIO.succeed(1)
    else
      for {
        last <- ZIO.suspendSucceed(fiboZIO_v2(n - 1))
        prev <- ZIO.suspendSucceed(fiboZIO_v2(n - 2))
      } yield last + prev

  def main(args: Array[String]): Unit = {
    val runtime        = Runtime.default
    given trace: Trace = Trace.empty

    Unsafe.unsafe { implicit u =>
      val mol = runtime.unsafe.run(meaningOfLife)
      println(mol)

      val last = runtime.unsafe.run(sequenceTakeLast(ZIO.succeed("first"), ZIO.succeed("last")))
      println(last)
      val first = runtime.unsafe.run(sequenceTakeFirst(ZIO.succeed("first"), ZIO.succeed("last")))
      println(first)

      val convertValue = runtime.unsafe.run(convert(ZIO.succeed(1), "2"))
      println(convertValue)

      val asUnitValue = runtime.unsafe.run(asUnit(ZIO.succeed(true)))
      println(asUnitValue)

      // runtime.unsafe.run(runForever(endlessLoop))

      val ten = runtime.unsafe.run(sumZIO(4))
      println(ten)
      val big = runtime.unsafe.run(sumZIO(200000))
      println(big)
      val big_v2 = runtime.unsafe.run(sumZIO_v2(2000000))
      println(big_v2)

      val one = runtime.unsafe.run(fiboZIO(2))
      println(one)
      val two = runtime.unsafe.run(fiboZIO(3))
      println(two)
      val three = runtime.unsafe.run(fiboZIO(4))
      println(three)
      val five = runtime.unsafe.run(fiboZIO(5))
      println(five)
      val eight = runtime.unsafe.run(fiboZIO(6))
      println(eight)
      val bigFibo = runtime.unsafe.run(fiboZIO(40))
      println(bigFibo)
      val bigFibo_v2 = runtime.unsafe.run(fiboZIO_v2(40))
      println(bigFibo_v2)
    }
  }
}
