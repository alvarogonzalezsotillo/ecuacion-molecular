package ecuacion

import org.scalajs.dom
import dom.document
import org.scalajs.jquery._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Left, Right}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global


object EcuacionMolecularApp {

  def log(s: String) = {}

  def main(args: Array[String]): Unit = {
    if( !scala.scalajs.js.isUndefined(document) ){
      jQuery(() => setupUI())
    }
    else{
      println( "Browser or nodejs required" )
    }
  }







  def setupUI(): Unit = {

    log( "setupUI" )
    val ecuacionNormalizadaDiv = jQuery("#ecuacion-normalizada")
    val ecuacionTex = jQuery("#ecuacion")
    val ejemplosDiv = jQuery("#ejemplos")
    val inicioElem = jQuery("#inicio")

    // LISTENER TEXTO DE ECUACION
    implicit val explicador = Explicador.default
    ecuacionTex.keyup{ () =>
      val s = ecuacionTex.value().toString
      log( "keyup:" + s )

      if( s.trim == "" ){
        ecuacionNormalizadaDiv.html("Introduce una ecuación, o selecciona un ejemplo");
      }
      else{
        val ec = EcuacionMolecular(s)
        val msg = ec.map(AjustadorEcuacionMolecular(_)) match {
          case Left(msg) =>
            ecuacionNormalizadaDiv.addClass("error")
            s"$msg"
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
    }

    def setupSamples() = {
      log( "setupSamples" )

      val ini = System.currentTimeMillis();

      // LISTENER DE CLICK EN EJEMPLO
      def ejemploClicked(e: JQueryEventObject, ignored: js.Any) = {
        val t = jQuery(e.target).closest("ejemplo")
        inicioElem.get(0).scrollIntoView(true)
        ecuacionNormalizadaDiv.html("Calculando...")
        ecuacionTex.value(t.text.toString)
        Future{
          ecuacionTex.keyup()
        }
      }

      // RELLENO DE EJEMPLOS
      for( e <- EcuacionMolecular.ejemplos ) {
        val ec = EcuacionMolecular(e).right.get
        log( "Ejemplo de " + ec )
        Future{
          val ejemplo = jQuery(s"<ejemplo>${ec.toHTML}</ejemplo")
          ejemplosDiv.append(ejemplo)
          ejemplo.click( ejemploClicked _ )
        }
      }


      val end = System.currentTimeMillis();
      log(s"Time: ${end-ini}ms")
    }

    setupSamples()

    Future{
      ecuacionTex.keyup()
    }
    

  }

}
