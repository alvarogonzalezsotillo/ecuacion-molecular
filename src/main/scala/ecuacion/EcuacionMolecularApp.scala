package ecuacion

import org.scalajs.dom
import dom.document
import org.scalajs.jquery._
import EcuacionMolecular.Ecuacion

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Left, Right}

object EcuacionMolecularApp {
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
    val ejemplosDiv = jQuery("#ejemplos")

    // LISTENER TEXTO DE ECUACION
    ecuacionTex.keyup{ () =>
      val s = ecuacionTex.value()
      val ec = EcuacionMolecular.parse(s.toString)
      val msg = ec.map(_.ajusta()) match {
        case Left(msg) => msg
        case Right(oec) => oec match {
          case Some(e) => e.toHTML
          case None => "No se puede ajustar la ecuación (índices muy altos o átomos no balanceables):" + ec
        }
      }
      ecuacionNormalizadaDiv.html(msg)
    }

    // RELLENO DE EJEMPLOS
    for( e <- EcuacionMolecular.ejemplos ) {
      val ec = EcuacionMolecular.parse(e).right.get
      ejemplosDiv.append(s"<ejemplo>${ec.toHTML}</ejemplo")
    }


    // LISTENER DE CLICK EN EJEMPLO
    val ejemplos = jQuery("ejemplo")
    ejemplos.click{ (e: JQueryEventObject, a: js.Any)=>
      val t = jQuery(e.target)
      ecuacionTex.value(t.text.toString)
      ecuacionTex.keyup()
    }
  }
}

