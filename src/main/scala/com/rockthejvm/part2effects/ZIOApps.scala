package com.rockthejvm.part2effects

import zio.*

object ZIOApps extends ZIOAppDefault {
  // provides runtime, trace, ...
  val meaningOfLife: UIO[Int] = ZIO.succeed(42)

  override def run = meaningOfLife.debug // meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))
}

// "not needed 99% of the time"
object ManualApp extends ZIOApp {
  override implicit def environmentTag = ???

  override type Environment = this.type

  override def bootstrap = ???

  override def run = ???
}
