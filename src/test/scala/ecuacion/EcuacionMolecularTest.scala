package ecuacion

import org.scalatest._



/**
  * Created by alvaro on 19/12/17.
  */
class EcuacionMolecularTest extends FlatSpec {

  it should "ajustar todas las ecuaciones de ejemplo" in {
    import EcuacionMolecular._



    for (e <- ejemplos) {
      try{
        println("******************************************************************************************")

        var explicacion = ""

        implicit val explicador = new Explicador{
          def explica( a: Any ) = explicacion += a.toString + "\n"
        }

        val ecuacion = EcuacionMolecular(e)
        val ecuacionAjustadaEither = ecuacion.map(AjustadorEcuacionMolecular(_))
        println( explicacion )
        val ecuacionAjustada = ecuacionAjustadaEither.right.get.get
        assert( ecuacionAjustada.esAjustada() )
      }
      catch{
        case e : Throwable => e.printStackTrace()
      }
    }
  }
}
