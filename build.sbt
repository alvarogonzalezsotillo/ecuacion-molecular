enablePlugins(ScalaJSPlugin)

name := "Scala.js Tutorial"
scalaVersion := "2.12.2" // or any other Scala version >= 2.10.2

// This is an application with a main method
mainClass in  (Compile, run) := Some("tutorial.webapp.TutorialApp")
scalaJSUseMainModuleInitializer := true

// DOM DESDE SCALA
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

// JQUERY
libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1"

// SCALATEST
libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0-M10" % "test"

// PARSERS
// NO CONSIGO QUE FUNCIONE, ME HE BAJADO DIRECTAMENTE LOS FUENTES
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.6"



// USAR WEBJARS PARA JQUERY
// esto crea scala-js-tutorial-jsdeps.js
skip in packageJSDependencies := false
jsDependencies += "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js"



