package com.rockthejvm.part2effects

import zio.*

object ZIODependencies extends ZIOAppDefault {

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] =
      for {
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase) =
      new UserSubscription(emailService, userDatabase)
  }

  class EmailService {
    def email(user: User): Task[Unit] =
      ZIO.succeed(println(s"You've just been subscribed to Rock the JVM. Welcome, ${user.name}!"))
  }
  object EmailService {
    def create() = new EmailService
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] =
      for {
        conn <- connectionPool.get
        _    <- conn.runQuery(s"INSERT INTO subscribers(name, email) VALUES (${user.name}, ${user.email})")
      } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool) =
      new UserDatabase(connectionPool)
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnections: Int) =
      new ConnectionPool(nConnections)
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] =
      ZIO.succeed(println(s"Executing query: $query"))
  }

  val subscriptionService = ZIO.succeed( // Dependency injection
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(
        ConnectionPool.create(10)
      )
    )
  )

  /*
   * "Clean DI" has drawbacks
   * - does not scale for many services
   * - DI can be 100x worse
   *   - pass dependencies partially
   *   - not having all dependencies in one place
   *   - passing dependencies multiple times
   */

  def subscribe(user: User): ZIO[Any, Throwable, Unit] = for {
    sub <- subscriptionService // service is instantiated at the point of call
    _   <- sub.subscribeUser(user)
  } yield ()

  // risk leaking resources if you subscribe multiple users in the same program
  val program = for {
    _ <- subscribe(User("Vinh", "vinh@vinh.com"))
    _ <- subscribe(User("John", "john@vinh.com"))
  } yield ()

  // alternative
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _   <- sub.subscribeUser(user)
  } yield ()

  val program_v2 = for {
    _ <- subscribe_v2(User("Vinh", "vinh@vinh.com"))
    _ <- subscribe_v2(User("John", "john@vinh.com"))
  } yield ()

  /*
   * - we don't need to care about dependencies until the end of the world
   * - all ZIOs requiring this dependency will use the same instance
   * - can use different instances of the same type for different needs (e.g. testing)
   * - layers can be created and composed much like regular ZIOs + rich API
   */
  override def run = program_v2.provideLayer(
    ZLayer.succeed(
      UserSubscription.create(
        EmailService.create(),
        UserDatabase.create(
          ConnectionPool.create(10)
        )
      )
    )
  )
}
