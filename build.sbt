organization := "com.github"

name := "freeap"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
  // "-Xfatal-warnings", // this makes cross compilation impossible from a single source
  "-Yno-adapted-args"
)

resolvers ++= Seq(Resolver.sonatypeRepo("releases"), Resolver.sonatypeRepo("snapshots"))

libraryDependencies ++= Seq(
  "org.scalaz"      %% "scalaz-core"                % "7.1.3",
  "org.scalaz"      %% "scalaz-concurrent"          % "7.1.3",
  "org.scalaz"      %% "scalaz-scalacheck-binding"  % "7.1.3"   % "test",
  "org.scalacheck"  %% "scalacheck"                 % "1.12.4"  % "test",
  "io.argonaut"     %% "argonaut"                   % "6.1-M4"
)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

initialCommands := "import scalaz._; import Scalaz._"