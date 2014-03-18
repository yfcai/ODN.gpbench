// CONFIGURATION

scalaSource in Test := baseDirectory.value

// LIBRARIES

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

scalaVersion := "2.10.3"
// scalaVersion := "2.11.0-M8" // alternatively ...

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
//  "com.chuusai" % "shapeless_2.10.3" % "2.0.0-M1" // alternatively ...
)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0"
