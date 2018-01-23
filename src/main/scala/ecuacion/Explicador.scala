package ecuacion

import scala.language.dynamics

trait Explicador{
  def siExplicadorActivo[T]( p : => T ) : T = {
    p
  }

  def explica( s: Any ) : Unit

  def explica( s1: Any, s2: Any, resto: Any* ) : Unit = {
    explica(s1)
    explica(s2)
    resto.foreach(explica)
  }
}

object Explicador{

  import scala.xml._

  val default = new Explicador {
    def explica(a: Any): Unit = println(a.toString())
  }

  def intercala( lis: Seq[Node], inter: Node) = lis.tail.foldLeft(Seq(lis.head)) { case (seq, g) =>
    seq :+ inter :+ g
  }
}
