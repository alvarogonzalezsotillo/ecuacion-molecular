package ecuacion

import org.scalatest._



/**
  * Created by alvaro on 19/12/17.
  */
class EcuacionMolecularTest extends FlatSpec {

  it should "ajustar todas las ecuaciones de ejemplo" in {
    import EcuacionMolecular._

    implicit val explicador = Explicador.default


    for (e <- ejemplos) {
      try{
        println("******************************************************************************************")
        println(e)
        val ecuacion = EcuacionMolecular(e)
        val ecuacionAjustadaEither = ecuacion.map(AjustadorEcuacionMolecular(_))
        println( ecuacionAjustadaEither )
        val ecuacionAjustada = ecuacionAjustadaEither.right.get.get
        println(ecuacionAjustada)
        assert( ecuacionAjustada.esAjustada() )
      }
      catch{
        case e : Throwable => e.printStackTrace()
      }
    }
  }
}
