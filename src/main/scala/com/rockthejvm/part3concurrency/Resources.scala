package com.rockthejvm.part3concurrency

import com.rockthejvm.utils.*
import zio.*

import java.io.File
import java.util.Scanner

object Resources extends ZIOAppDefault {

  // finalizers
  def unsafeMethod(): Int = throw new RuntimeException("Not an int here for you!")
  val anAttempt           = ZIO.attempt(unsafeMethod())

  // finalizers (they finish before the effect is finished)
  val attemptWithFinalizer = anAttempt.ensuring(ZIO.succeed("finalizer!").debugThread)
  // multiple finalizers (in order)
  val attemptWith2Finalizers = attemptWithFinalizer.ensuring(ZIO.succeed("another finalizer!").debugThread)
  // .onInterrupt, .onError, .onDone, .onExit

  // resource lifecycle
  class Connection(url: String) {
    def open()  = ZIO.succeed(s"Opening connection to $url...").debugThread
    def close() = ZIO.succeed(s"Closing connection to $url...").debugThread
  }

  object Connection {
    def create(url: String) = ZIO.succeed(new Connection(url))
  }

  val fetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib  <- (conn.open() *> ZIO.sleep(300.seconds)).fork
    _    <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _    <- fib.join
  } yield () // resource leak

  val correctFetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib  <- (conn.open() *> ZIO.sleep(300.seconds)).ensuring(conn.close()).fork
    _    <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _    <- fib.join
  } yield () // preventing leaks

  // tedious

  // acquireRelease
  // - acquiring cannot be interrupted
  // - all finalizers are guaranteed to run
  val cleanConnection = ZIO.acquireRelease(Connection.create("rockthejvm.com"))(_.close())
  val fetchWithResource =
    for {
      conn <- cleanConnection
      fib  <- (conn.open() *> ZIO.sleep(300.seconds)).fork
      _    <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
      _    <- fib.join
    } yield ()

  // To eliminate scope dependency
  val fetchWithScopedResource = ZIO.scoped(fetchWithResource)

  // acquireReleaseWIth
  val cleanConnection_v2 = ZIO.acquireReleaseWith(
    Connection.create("rockthejvm.com") // acquire
  )(
    _.close()                                     // release
  )(conn => conn.open() *> ZIO.sleep(300.seconds) // use
  )

  val fetchWithResource_v2 =
    for {
      fib <- cleanConnection_v2.fork
      _   <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
      _   <- fib.join
    } yield ()

  /*
   * Exercises
   * 1. Use the acquireRelease to open a file, print all lines, (one every 100 millis), then close the file
   */
  def openFileScanner(path: String): UIO[Scanner] =
    ZIO.succeed(new Scanner(new File(path)))

  def printLinesWithDelay(scanner: Scanner, duration: Duration): UIO[Unit] =
    if (scanner.hasNextLine)
      ZIO.sleep(duration) *> ZIO.succeed(scanner.nextLine()).debugThread *> printLinesWithDelay(scanner, duration)
    else ZIO.unit

  def acquireOpenFile(path: String): UIO[Unit] =
    for {
      scanner <- ZIO.succeed(s"opening file $path").debugThread *>
        ZIO.acquireReleaseWith(
          openFileScanner(path)
        )(scanner => ZIO.succeed(s"closing file at $path").debugThread *> ZIO.succeed(scanner.close()))(
          printLinesWithDelay(_, 100.millis)
        )
    } yield ()

  val testInterruptFileDisplay = for {
    fib <- acquireOpenFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala").fork
    _   <- ZIO.sleep(2.seconds) *> fib.interrupt
  } yield ()

  // acquireRelease vs acquireReleaseWith
  def connFromConfig(path: String): UIO[Unit] =
    ZIO.acquireReleaseWith(openFileScanner(path))(scanner =>
      ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close())
    ) { scanner =>
      ZIO.acquireReleaseWith(Connection.create(scanner.nextLine()))(_.close()) { conn =>
        conn.open() *> ZIO.never
      }
    }

  // nested resource
  def connFromConfig_v2(path: String): UIO[Unit] = ZIO.scoped {
    for {
      scanner <- ZIO.acquireRelease(openFileScanner(path))(scanner =>
        ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close())
      )
      conn <- ZIO.acquireRelease(Connection.create(scanner.nextLine()))(_.close())
      _    <- conn.open() *> ZIO.never
    } yield ()
  }

  def run = connFromConfig_v2("src/main/resources/connection.conf")
}
