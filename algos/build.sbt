scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.6",
  "commons-codec" % "commons-codec" % "1.12",
  "org.json4s" %% "json4s-jackson" % "3.6.5",
  "com.typesafe.akka" %% "akka-actor" % "2.5.22",
  "javax.xml.bind" % "jaxb-api" % "2.3.1",
  "org.scalatest" %% "scalatest" % "3.1.0-RC1" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test
)
