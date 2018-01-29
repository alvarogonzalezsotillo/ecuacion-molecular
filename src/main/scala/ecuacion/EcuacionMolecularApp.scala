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

    import scala.xml._

    log( "setupUI" )
    val ecuacionNormalizadaDiv = jQuery("#ecuacion-normalizada")
    val ecuacionTex = jQuery("#ecuacion")
    val ejemplosDiv = jQuery("#ejemplos")
    val inicioElem = jQuery("#inicio")


    case class ResultadoAjustaEcuacion( msg: String, error: Boolean, explicacion: String )

    def ajustaEcuacion( s : String ) : ResultadoAjustaEcuacion = {


      class Expl extends Explicador{
        var s = ""
        def explica( a : Any ) = s += a.toString + "\n"
        override def toString() = s
      }

      if( s.trim == "" ){
        ResultadoAjustaEcuacion("Introduce una ecuación, o selecciona un ejemplo",false,"")
      }
      else{

        EcuacionMolecular(s) match{
          case Left(msg) =>
            ResultadoAjustaEcuacion(msg,true,"")
          case Right(ec) =>
            implicit val explicador = new Expl()
            AjustadorEcuacionMolecular(ec) match{
              case Some(ecAjustada) =>
                ResultadoAjustaEcuacion(ecAjustada.toHTML,false,explicador.toString)
              case None =>
                ResultadoAjustaEcuacion("No se puede ajustar la ecuación",true,explicador.toString)
            }

        }
      }
      
    }

    // LISTENER TEXTO DE ECUACION
    ecuacionTex.keyup{ () =>
      val s = ecuacionTex.value().toString
      log( "keyup:" + s )

      val ResultadoAjustaEcuacion(ecuacion,error,explicacion) = ajustaEcuacion(s)

      if( error )
        ecuacionNormalizadaDiv.addClass("error")
      else
        ecuacionNormalizadaDiv.removeClass("error")

      ecuacionNormalizadaDiv.html(ecuacion)
      agregaExplicacion(ecuacionNormalizadaDiv,explicacion)
     
    }

    def agregaExplicacion( div: JQuery, explicacion: String ) = {
      val boton = jQuery(<a href="#">ver explicación</a>.toString)
      val newDiv = jQuery(<div class="explicacion"></div>.toString)
      println( boton )
      newDiv.append(explicacion)
      boton.appendTo(div)
      newDiv.appendTo(div)

      boton.click{() =>
        newDiv.toggleClass("visible")
      }

    }

    def setupSamples() = {
      log( "setupSamples" )

      val ini = System.currentTimeMillis();

      // LISTENER DE CLICK EN EJEMPLO
      def ejemploClicked(e: JQueryEventObject, ignored: js.Any) : Unit = {
        val t = jQuery(e.target).closest("ejemplo")
        inicioElem.get(0).scrollIntoView(true)
        ecuacionNormalizadaDiv.html("Calculando...")
        ecuacionTex.value(t.text.toString.replace( " ", "").replace("="," = "))
        Future{
          ecuacionTex.keyup()
        }
      }


      // RELLENO DE EJEMPLOS
      for( e <- EcuacionMolecular.ejemplos ) {
        val ec = EcuacionMolecular(e).right.get
        log( "Ejemplo de " + ec )
        Future{
          val ejemplo = jQuery( <ejemplo>{ec.toXML}</ejemplo>.toString() )
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
