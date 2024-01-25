package com.rockthejvm.utils

import zio.ZIO

extension [R, E, A](zio: ZIO[R, E, A])
  def debugThread: ZIO[R, E, A] =
    zio
      .tap(value => ZIO.succeed(println(s"[${Thread.currentThread().getName}] $value")))
      .tapErrorCause(error => ZIO.succeed(println(s"[${Thread.currentThread().getName}][FAIL] $error")))
