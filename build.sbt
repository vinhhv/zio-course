name         := "zio"
version      := "0.1"
scalaVersion := "3.3.1"

lazy val zioVersion = "2.0.21"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"            % zioVersion,
  "dev.zio" %% "zio-test"       % zioVersion,
  "dev.zio" %% "zio-test-sbt"   % zioVersion,
  "dev.zio" %% "zio-streams"    % zioVersion,
  "dev.zio" %% "zio-test-junit" % zioVersion
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
