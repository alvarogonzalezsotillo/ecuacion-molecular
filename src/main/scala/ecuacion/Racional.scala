package ecuacion

import ecuacion.Racional.Numero

import scala.annotation.tailrec

/**
  * Created by alvaro on 27/12/17.
  */
class Racional(num_ : Numero, den_ : Numero ){

  import Racional._
  import Math.abs

  assert( den_ != 0 )

  val num = Math.signum(den_).toInt * num_ / mcd(abs(num_),abs(den_))
  val den = Math.abs( den_ / mcd(abs(num_),abs(den_)) )

  def plus( r: Racional ) = Racional( num*r.den + r.num*den, den*r.den )

  lazy val negate = Racional( -num, den )

  def minus( r: Racional ) = plus( r.negate )

  def times( r: Racional ) = Racional( num*r.num, den * r.den )

  def div( r: Racional ) = Racional( num*r.den, den * r.num )

  def compare( r: Racional ) = minus(r).num.toInt

  override lazy val toString = if( num == 0 ) "0" else if( den == 1 ) s"$num" else s"$num/$den"

  override def equals( r: Any ) = r match {
    case r: Racional => compare(r) == 0
    case a => this == a
  }

  lazy val toInt = num/den
  lazy val toLong = 1L*num/den
  lazy val toFloat = toDouble.toFloat
  lazy val toDouble = 1.0F * num/den
}


object Racional {

  type Numero = Int

  def apply(num: Numero, den: Numero = 1): Racional = new Racional(num, den)

  @tailrec
  def mcd( a: Numero, b: Numero ) : Numero = if( b == 0 ) a else mcd( b, a % b )

  def mcd( s: Seq[Numero] ) : Numero = {
    s.foldLeft( s.head )( (x,y) => mcd(x,y) )
  }

  def mcm( a: Numero, b: Numero ) = a*b/mcd(a,b)
  def mcm( s: Seq[Numero] ) : Numero = s.foldLeft( s.head )( (x,y) => mcm(x,y) )




  object Implicits{
    implicit def intToRational(i: Numero) = Racional(i)
    implicit class IntConstructor(i:Numero){
      def \\ (den: Numero) = Racional(i,den)
    }
  }

  implicit object FractionalRacional extends Fractional[Racional] {
    override def div(x: Racional, y: Racional): Racional = x.div(y)

    override def plus(x: Racional, y: Racional): Racional = x.plus(y)

    override def minus(x: Racional, y: Racional): Racional = x.minus(y)

    override def times(x: Racional, y: Racional): Racional = x.times(y)

    override def negate(x: Racional): Racional = x.negate

    override def fromInt(x: Int): Racional = Racional(x)

    override def toInt(x: Racional): Int = x.toInt

    override def toLong(x: Racional): Long = x.toLong

    override def toFloat(x: Racional): Float = x.toFloat

    override def toDouble(x: Racional): Double = x.toDouble

    override def compare(x: Racional, y: Racional): Int = {
      val diff = x.minus(y)
      diff.toInt
    }
  }

}
