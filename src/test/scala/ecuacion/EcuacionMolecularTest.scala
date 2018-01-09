import org.scalatest._

package ecuacion

/**
  * Created by alvaro on 19/12/17.
  */
object EcuacionMolecularTest extends FlatSpec {

  it should " ajustar todas las ecuaciones de ejemplo" in {
    import EcuacionMolecular._

    for (e <- ejemplos) {
      println("******************************************************************************************")
      println(e)
      val ecuacion = EcuacionMolecular(e)
      println(ecuacion.right.get)
      val ecuacionAjustada = ecuacion.map(AjustadorEcuacionMolecular(_))
      println(ecuacionAjustada.right.get.get)

      assert( ecuacionAjustada.esAjustada() )
    }
  }

  def testSecuencia = {
    import Secuencias._
    for (s <- Secuencias.iterator(3, 3)) println(s.mkString(","))
  }

}
