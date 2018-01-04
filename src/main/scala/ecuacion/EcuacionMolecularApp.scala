package ecuacion

import org.scalajs.dom
import dom.document
import org.scalajs.jquery._

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
    val inicioElem = jQuery("#inicio")

    // LISTENER TEXTO DE ECUACION
    ecuacionTex.keyup{ () =>
      val s = ecuacionTex.value()
      val ec = EcuacionMolecular(s.toString)
      val msg = ec.map(AjustadorEcuacionMolecular(_)) match {
        case Left(msg) =>
          ecuacionNormalizadaDiv.addClass("error")
          s"Introduce una ecuación, o selecciona un ejemplo ($msg)"
        case Right(oec) => oec match {
          case Some(e) =>
            ecuacionNormalizadaDiv.removeClass("error")
            e.toHTML
          case None =>
            ecuacionNormalizadaDiv.addClass("error")
            s"No se puede ajustar la ecuación (índices muy altos o átomos no balanceables):$ec"
        }
      }
      ecuacionNormalizadaDiv.html(msg)
    }

    // RELLENO DE EJEMPLOS
    for( e <- EcuacionMolecular.ejemplos ) {
      val ec = EcuacionMolecular(e).right.get
      ejemplosDiv.append(s"<ejemplo>${ec.toHTML}</ejemplo")
    }


    // LISTENER DE CLICK EN EJEMPLO
    val ejemplos = jQuery("ejemplo")
    ejemplos.click{ (e: JQueryEventObject, _: js.Any)=>
      val t = jQuery(e.target).closest("ejemplo")
      inicioElem.get(0).scrollIntoView(true)
      ecuacionNormalizadaDiv.html("Calculando...")
      ecuacionTex.value(t.text.toString)
      dom.window.setTimeout( { () =>
        ecuacionTex.keyup()
      }, 50)
    }
  }
}

