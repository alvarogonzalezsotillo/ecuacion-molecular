package ecuacion

object XmlTest extends App{

  import scala.xml._

  val nodeseq : NodeSeq = Seq( <p/>, <div/> )

  val a = <p> { nodeseq } </p>

  println( a.toString() )

}
