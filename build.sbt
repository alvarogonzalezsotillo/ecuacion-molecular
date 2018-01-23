enablePlugins(ScalaJSPlugin)

name := "Ajustador de ecuacion"
scalaVersion := "2.12.2" // or any other Scala version >= 2.10.2

// This is an application with a main method
mainClass in  (Compile, run) := Some("tutorial.webapp.TutorialApp")
scalaJSUseMainModuleInitializer := true

// DOM DESDE SCALA
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

// JQUERY
libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1"

// SCALATEST
libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test"

// PARSERS
// NO CONSIGO QUE FUNCIONE CON LA 1.0.6 SIN BAJAR DIRECTAMENTE LOS FUENTES, ME QUEDO CON LA 1.0.5
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5"

// NO HAY LIBRERÍA PARA XML PARA SCALAJS
// HE PUESTO LOS FUENTES EN SRC
// DE TODAS FORMAS, HACE FALTA ESTA DEPENDENCIA PARA QUE FUNCIONE EL COMPILADOR
// https://stackoverflow.com/questions/41830090/scala-js-support-for-scala-xml-and-scala-compiler
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"


// USAR WEBJARS PARA JQUERY
// esto crea scala-js-tutorial-jsdeps.js
skip in packageJSDependencies := false
jsDependencies += "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js"



