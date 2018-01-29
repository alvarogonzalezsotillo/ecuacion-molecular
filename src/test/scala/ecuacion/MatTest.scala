package ecuacion

import org.scalatest._

import scala.util.Try

class MatTest extends FlatSpec {


  implicit val explicador = ecuacion.Explicador.default

  it should "Resolver un sistema definido" in {
    import EcuacionMolecular._
    import ecuacion.Racional._
    import ecuacion.Racional.Implicits._

    val m = new Mat( Array (
      Array(1\\1, 2\\1, 3\\1, 3\\1),
      Array(3\\1, 2\\1, 1\\1, 4\\1),
      Array(2\\1, 3\\1, 1\\1, 4\\1)
    ))

    val variables = m.solve.get
    println( "Las variables son:" + variables.mkString(", ") )
    assert( variables(0) == 3\\4)
    assert( variables(1) == 3\\4)
    assert( variables(2) == 1\\4)
  }

  it should "Fallar en un sistema sin solución" in {
    import EcuacionMolecular._

    val m = new Mat( Array (
      Array(1.0, 2.0, 3.0, 4.0),
      Array(3.0, 2.0, 1.0, 4.0),
      Array(2.0, 4.0, 6.0, 4.0)
    ))

    val t = Try(m.solve)
    assert(t.isFailure)
  }

  it should "Fallar en un sistema indefinido" in {
    import EcuacionMolecular._

    val m = new Mat( Array (
      Array(1.0, 2.0, 3.0, 4.0),
      Array(3.0, 2.0, 1.0, 4.0),
      Array(2.0, 4.0, 6.0, 8.0)
    ))

    val t = Try(m.solve)
    assert(t.isFailure)
  }


}