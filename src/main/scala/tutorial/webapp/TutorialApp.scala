package tutorial.webapp


import org.scalajs.dom
import dom.document
import org.scalajs.jquery._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Left, Right}

object TutorialApp {
  def main(args: Array[String]): Unit = {
    if( !scala.scalajs.js.isUndefined(document) ){
      jQuery(() => setupUI())
    }
    else{
      println( "Browser or nodejs required" )
    }
  }


  def setupUI(): Unit = {

    val ecuacionNormalizadaDiv = jQuery("#ecuacion-normalizada")
    val ecuacionTex = jQuery("#ecuacion")

    ecuacionTex.keyup{ () =>
      val s = ecuacionTex.value()
      val ec = EcuacionMolecular.parse(s.toString)
      val msg = ec.map(_.ajusta()) match {
        case Left(msg) => msg
        case Right(oec) => oec match {
          case Some(e) => e.toString
          case None => "No se puede ajustar la ecuación (índices muy altos o átomos no balanceables):" + ec
        }
      }
      ecuacionNormalizadaDiv.text(msg)
    }

    val ejemplos = jQuery("ejemplo")
    ejemplos.click{ (e: JQueryEventObject, a: js.Any)=>
      val t = jQuery(e.target)
      ecuacionTex.value(t.text.toString)
      ecuacionTex.keyup()
    }
  }
}

