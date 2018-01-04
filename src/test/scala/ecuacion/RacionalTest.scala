package ecuacion

import ecuacion.Racional.FractionalRacional

/**
  * Created by alvaro on 27/12/17.
  */
object RacionalTest extends App{

  import Racional.Implicits._
  import Fractional.Implicits._


  val r = 1\\3 + 1\\4

  println( r )
  assert( r == 7\\12 )

  println( -1\\3 )
  assert( (-1\\3).num == -1 )
  assert( (-1\\3).den == 3 )

  println( -1\\ -3 )
  assert( (-1\\ -3).num == 1 )
  assert( (-1\\ -3).den == 3 )


}
