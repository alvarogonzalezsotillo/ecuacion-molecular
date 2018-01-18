package ecuacion

import scala.language.dynamics

trait Explicador extends (Seq[String] => Unit) with Dynamic{
  def __print( a : Any ) : Unit
  def apply(a : Seq[String]) = a.foreach( __print )
  def applyDynamic(methodName: String)(args: Any*) {
    args.map(_.toString).foreach(__print)
  }

  def siExplicadorActivo[T]( p : => T ) : T = {
    p
  }

  def explica( a: Any ) = __print(a)
}

object Explicador{
  val default = new Explicador {
    def __print(a: Any): Unit = println(a.toString())
  }
}
