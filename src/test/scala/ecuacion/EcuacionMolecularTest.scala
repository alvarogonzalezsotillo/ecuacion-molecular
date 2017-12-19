package ecuacion

/**
  * Created by alvaro on 19/12/17.
  */
object EcuacionMolecularTest extends App {

  def testEcuacion = {
    import EcuacionMolecular._

    for (e <- ejemplos.par) {
      val ecuacion = EcuacionMolecular.parse(e)
      val ecuacionAjustada = ecuacion.map(_.ajusta())
      println("****************")
      println(e)
      println(ecuacion.right.get)
      println(ecuacionAjustada.right.get.get)
    }
  }

  def testSecuencia = {
    import Secuencias._
    for (s <- Secuencias.iterator(3, 3)) println(s.mkString(","))
  }

  testEcuacion

}
