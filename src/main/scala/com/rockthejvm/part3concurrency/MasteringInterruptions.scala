package com.rockthejvm.part3concurrency

import com.rockthejvm.utils.*
import zio.*

object MasteringInterruptions extends ZIOAppDefault {

  // interruptions:
  // fib.interrupt
  // ZIO.race, ZIO.zipPar, ZIO.collectAllPar
  // outliving parent fiber

  // manual interruptions
  val aManuallyInterruptedZIO = ZIO.succeed("computing...").debugThread *> ZIO.interrupt *> ZIO.succeed(42).debugThread

  // finalizer
  val effectWithInterruptionFinalizer =
    aManuallyInterruptedZIO.onInterrupt(ZIO.succeed("I was interrupted!").debugThread)

  // uninterruptible
  // payment flow to NOT be interrupted
  val fussyPaymentSystem = (
    ZIO.succeed("Payment running, don't cancel me...").debugThread *>
      ZIO.sleep(1.second) *> // the actual payment
      ZIO.succeed("Payment completed").debugThread
  ).onInterrupt(ZIO.succeed("MEGA CANCEL OF DOOM").debugThread) // don't want this triggered

  val cancellationOfDoom = for {
    fib <- fussyPaymentSystem.fork
    _   <- ZIO.sleep(500.millis) *> fib.interrupt
    _   <- fib.join
  } yield ()

  // ZIO.uninterruptible
  val atomicPayment    = ZIO.uninterruptible(fussyPaymentSystem) // make a ZIO atomic
  val atomicPayment_v2 = fussyPaymentSystem.uninterruptible      // same
  val noCancellationOfDoom = for {
    fib <- atomicPayment_v2.fork
    _   <- ZIO.sleep(500.millis) *> fib.interrupt
    _   <- fib.join
  } yield ()

  // interruptibility is regional
  val zio1         = ZIO.succeed(1)
  val zio2         = ZIO.succeed(2)
  val zio3         = ZIO.succeed(3)
  val zioComposed  = (zio1 *> zio2 *> zio3).uninterruptible               // All the ZIOs are uninterruptible
  val zioComposed2 = (zio1 *> zio2.interruptible *> zio3).uninterruptible // inner scopes override outer scopes

  // uninterruptibleMask
  // example: an authentication service
  // - input password, can be interrupted, because otherwise it might block the fiber indefinitely
  // - verify password, which cannot be interrupted once it's triggered
  val inputPassword = for {
    _    <- ZIO.succeed("Input password: ").debugThread
    _    <- ZIO.succeed("(typing password)").debugThread
    _    <- ZIO.sleep(2.seconds)
    pass <- ZIO.succeed("RockTheJVM1!")
  } yield pass

  def verifyPassword = (pw: String) =>
    for {
      _      <- ZIO.succeed("verifying...").debugThread
      _      <- ZIO.sleep(2.seconds)
      result <- ZIO.succeed(pw == "RockTheJVM1!")
    } yield result

  val authFlow = ZIO.uninterruptibleMask { restore =>
    for {
      pw <- restore(inputPassword).onInterrupt(ZIO.succeed("Authentication timed out. Try again later.").debugThread)
      verification <- verifyPassword(pw)
      _ <-
        if (verification) ZIO.succeed("Authentication successful.").debugThread
        else ZIO.succeed("Authentication failed.").debugThread
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.fork
    _ <- ZIO.sleep(3.seconds) *> ZIO.succeed("Attempting to cancel authentication...").debugThread *> authFib.interrupt
    _ <- authFib.join
  } yield ()

  /**
   * Exercises
   */
  val cancelBeforeMol   = ZIO.interrupt *> ZIO.succeed(42).debugThread
  val uncancelBeforeMol = ZIO.uninterruptible(ZIO.interrupt *> ZIO.succeed(42).debugThread)
  // neither will print, the 2nd one has an inner scoped interrupt, which takes precedence

  // 2
  val authProgram_v2 =
    for {
      // fills in the interruptible gaps, making this entire thing uninterruptible. Be careful with nested .uninterruptible/.uninterruptibleMask
      authFib <- ZIO.uninterruptibleMask(_ => authFlow).fork
      _ <- ZIO
        .sleep(3.seconds) *> ZIO.succeed("Attempting to cancel authentication...").debugThread *> authFib.interrupt
      _ <- authFib.join
    } yield ()

  // 3
  val threeStepProgram = {
    val sequence = ZIO.uninterruptibleMask { restore =>
      for {
        _ <- restore(ZIO.succeed("interruptible").debugThread *> ZIO.sleep(1.second))
        _ <- ZIO.succeed("uninterruptible").debugThread *> ZIO.sleep(1.second)
        _ <- restore(ZIO.succeed("interruptible 2").debugThread *> ZIO.sleep(1.second))
      } yield ()
    }

    for {
      fib <- sequence.fork
      _   <- ZIO.sleep(1500.millis) *> ZIO.succeed("INTERRUPTING!").debugThread *> fib.interrupt
      _   <- fib.join
    } yield ()
  }

  def run = threeStepProgram
}
