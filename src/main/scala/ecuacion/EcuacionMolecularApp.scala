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

  def log(s: String) = println(s)

  def main(args: Array[String]): Unit = {
    log( "En el main")
    if( !scala.scalajs.js.isUndefined(document) ){
      jQuery(() =>setupUI())
    }
    else{
      println( "Browser or nodejs required" )
    }
    log( "Se acaba el main")
  }







  def setupUI(): Unit = {

    log( "setupUI" )
    val ecuacionNormalizadaDiv = jQuery("#ecuacion-normalizada")
    val ecuacionTex = jQuery("#ecuacion")
    val ejemplosDiv = jQuery("#ejemplos")
    val explicacionDiv = jQuery("#explicacion")
    val inicioElem = jQuery("#inicio")



    def ajustaEcuacion( s : String ) : (String,Boolean,String) = {


      class Expl extends Explicador{
        var s = ""
        def explica( a : Any ) = s += a.toString + "\n"
        override def toString() = s
      }

      if( s.trim == "" ){
        ("Introduce una ecuación, o selecciona un ejemplo",false,"")
      }
      else{

        EcuacionMolecular(s) match{
          case Left(msg) =>
            (msg,true,"")
          case Right(ec) =>
            implicit val explicador = new Expl()
            AjustadorEcuacionMolecular(ec) match{
              case Some(ecAjustada) =>
                (ecAjustada.toHTML,false,explicador.toString)
              case None =>
                ("No se puede ajustar la ecuación",true,explicador.toString)
            }

        }
      }
      
    }

    // LISTENER TEXTO DE ECUACION
    ecuacionTex.keyup{ () =>
      val s = ecuacionTex.value().toString
      log( "keyup:" + s )

      val (ecuacion,error,explicacion) = ajustaEcuacion(s)

      if( error )
        ecuacionNormalizadaDiv.addClass("error")
      else
        ecuacionNormalizadaDiv.removeClass("error")

      ecuacionNormalizadaDiv.html(ecuacion)
      explicacionDiv.html(explicacion)
      
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
