name := "JsonSql"

version := "0.1"

scalaVersion := "2.13.1"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % "1.5.13",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalactic" %% "scalactic" % "3.1.0",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test"
)
