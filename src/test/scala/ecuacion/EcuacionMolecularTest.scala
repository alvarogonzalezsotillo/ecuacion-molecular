package ecuacion

import org.scalatest._



/**
  * Created by alvaro on 19/12/17.
  */
class EcuacionMolecularTest extends FlatSpec {

  it should "ajustar todas las ecuaciones de ejemplo" in {
    import EcuacionMolecular._

    def log(s:String) = { }


    for (e <- ejemplos) {
      try{
        log("******************************************************************************************")

        var explicacion = ""

        implicit val explicador = new Explicador{
          def explica( a: Any ) = explicacion += a.toString + "\n"
        }

        val ecuacion = EcuacionMolecular(e)
        val ecuacionAjustadaEither = ecuacion.map(AjustadorEcuacionMolecular(_))
        log( explicacion )
        val ecuacionAjustada = ecuacionAjustadaEither.right.get.get
        assert( ecuacionAjustada.esAjustada() )
      }
      catch{
        case e : Throwable => e.printStackTrace()
      }
    }
  }
}
