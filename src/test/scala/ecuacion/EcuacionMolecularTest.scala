package ecuacion

import org.scalatest._



/**
  * Created by alvaro on 19/12/17.
  */
class EcuacionMolecularTest extends FlatSpec {

  it should "ajustar todas las ecuaciones de ejemplo" in {
    import EcuacionMolecular._

    def log(s:String) = { println(s) }


    for (e <- ejemplos) {
        log("******************************************************************************************")
        log( s"e:$e")

        var explicacion = ""

        implicit val explicador = new Explicador{
          def explica( a: Any ) = explicacion += a.toString + "\n"
        }

        val ecuacion = EcuacionMolecular(e)
        log( s"ecuacion:$ecuacion")
        assert( ecuacion.isRight )
        val ecuacionAjustadaO = AjustadorEcuacionMolecular( ecuacion.right.get )
        //log( s"ecuacionAjustadaO:$ecuacionAjustadaO")
        //log( explicacion )
        assert( ecuacionAjustadaO.isDefined )
        val ecuacionAjustada = ecuacionAjustadaO.get
        log( s"ecuacionAjustada:$ecuacionAjustada")
        assert( ecuacionAjustada.esAjustada() )
    }
  }
}
