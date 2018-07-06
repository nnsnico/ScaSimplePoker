name := "ScaSimplePoker"

version := "0.1"

lazy val catsVersion = "1.1.0"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion
)