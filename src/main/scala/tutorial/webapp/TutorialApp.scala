package tutorial.webapp


import org.scalajs.dom
import dom.document
import org.scalajs.jquery.jQuery

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object TutorialApp {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println( document )
    if( !scala.scalajs.js.isUndefined(document) ){
      appendPar(document.body, "Hello World")
      jQuery(() => setupUI())
    }
  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }

  @JSExportTopLevel("addClickedMessage")
  def addClickedMessage(): Unit = {
    val s = jQuery("#ecuacion").value()
    appendPar(document.body, "You clicked the button! : " + s)
  }

  def setupUI(): Unit = {
    jQuery("#click-me-button").click( () => addClickedMessage() )
    jQuery("body").append("<p>Hello World</p>")

    val ecuacionNormalizadaDiv = jQuery("#ecuacion-normalizada")

    jQuery("#ecuacion").keyup{ () =>
      val s = jQuery("#ecuacion").value()
      val (success,norm) = EcuacionMolecular.ajustaEcuacion(s.toString)
      norm match {
        case Some(s) =>
          println(s)
          ecuacionNormalizadaDiv.text(s)
        case None =>
          println("No puede ajustarse")
          ecuacionNormalizadaDiv.text("No puede ajustarse")
      }
    }
  }
}

