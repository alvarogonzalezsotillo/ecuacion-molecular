
package ecuacion

import org.scalatest._


/**
  * Created by alvaro on 27/12/17.
  */
class RacionalTest extends FlatSpec{

  import ecuacion.Racional.FractionalRacional
  import Racional.Implicits._
  import Fractional.Implicits._

  it should "sumar" in {
    assert( 1\\3 + 1\\4 == 7\\12 )
  }

  it should "llevar el signo en el denominador" in {
    assert( (-1\\3).num == -1 )
    assert( (-1\\3).den == 3 )

    assert( (-1\\ -3).num == 1 )
    assert( (-1\\ -3).den == 3 )
  }
 
  "Jaime" should "ver esto" in{
    println( "Hola Jaime");
  }

}
