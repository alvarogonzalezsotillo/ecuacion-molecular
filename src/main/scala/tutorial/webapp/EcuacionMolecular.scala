package tutorial.webapp

import java.io.{PrintStream, Serializable, Writer}

import scala.Option
import scala.collection.mutable.IndexedSeq
import scala.util.{Left, Right}
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by alvaro on 12/11/17.
  */

object EcuacionMolecular{

  case class AtomoEnMolecula(elemento: String, cantidad: Int = 1)

  case class Molecula(val atomos: List[AtomoEnMolecula], val cantidad: Int = 1){
    def atomosDeMolecula( multiplier: Int = 1 ) : Map[String,Int] = {
      atomos.map( am => am.elemento -> am.cantidad * cantidad * multiplier ).toMap
    }
  }

  case class LadoEcuacion(moleculas: List[Molecula]){
    def atomosDeLadoDeEcuacion( multipliers: Option[List[Int]] = None ) : Map[String,Int] = {
      val multipliers_ = multipliers.getOrElse(Iterator.continually(1).take(moleculas.size).toList)
      val maps = moleculas.zip(multipliers_).map{ case (mol, mul) => mol.atomosDeMolecula(mul) }
      maps.foldLeft( Map[String,Int]() ){ (m,accum) =>
        val keys = m.keySet ++ accum.keySet
        keys.map( k => k -> (m.getOrElse(k,0) + accum.getOrElse(k,0) ) ).toMap
      }
    }

  }

  case class Ecuacion(ladoIzquierdo: LadoEcuacion, ladoDerecho: LadoEcuacion) {


    override def toString: String = {

      def toStringC(i: Int) = if (i > 1) i.toString else ""

      def toStringA(a: AtomoEnMolecula) = a.elemento + toStringC(a.cantidad)

      def toStringM(m: Molecula) = toStringC(m.cantidad) + m.atomos.map(toStringA(_)).mkString


      def toStringL(l: LadoEcuacion) = {
        l.moleculas.map(toStringM(_)).mkString(" + ")
      }

      toStringL(ladoIzquierdo) + " = " + toStringL(ladoDerecho)
    }

    def esAjustada(multipliers: List[Int]): Boolean = {
      val ni = ladoIzquierdo.moleculas.size
      val nd = ladoDerecho.moleculas.size
      assert(ni + nd == multipliers.size)
      val (mi, md) = multipliers.splitAt(ni)
      val ai = ladoIzquierdo.atomosDeLadoDeEcuacion(Some(mi))
      val ad = ladoDerecho.atomosDeLadoDeEcuacion(Some(md))
      ai == ad
    }

    def ajusta(maxSum: Int = 100): Option[Ecuacion] = {
      val it_ = Secuencias.iterator(ladoDerecho.moleculas.size + ladoIzquierdo.moleculas.size, maxSum)
      val it = it_.map { l => l.map(_ + 1) }
      val multipliers = it.find(esAjustada(_))


      multipliers.map { m =>
        val (mi, md) = m.splitAt(ladoIzquierdo.moleculas.size)
        val li = ladoIzquierdo.moleculas.zip(mi).map { case (mol, mul) =>
          Molecula(mol.atomos, mol.cantidad * mul)
        }
        val ld = ladoDerecho.moleculas.zip(md).map { case (mol, mul) =>
          Molecula(mol.atomos, mol.cantidad * mul)
        }
        Ecuacion(LadoEcuacion(li), LadoEcuacion(ld))
      }
    }
  }


  def parse( s: String ) :  String Either Ecuacion = {
    val parser = new EcuacionMolecularParser
    import parser._
    parser.parse(parser.ecuacion, s) match{
      case Success(ecuacion, _) => Right(ecuacion)
      case NoSuccess(msg,_) => Left(msg)
    }
  }


}

object Secuencias{


  private def primero( a: IndexedSeq[Int], sum: Int ){
    a(0) = sum
    for( i <- 1 until a.size ) a(i) = 0
  }

  private def siguiente( a: Array[Int], sum: Int, pos: Int = 0 ) : Boolean = {
    assert( pos < a.length )
    if( pos < a.length-1) {
      // INTENTO CAMBIAR LA COLA
      if (siguiente(a, sum - a(pos), pos + 1)) {
        return true
      }

      // SI NO CAMBIA, DEBO BAJAR UNA UNIDAD LA POSICION
      if( a(pos) > 0 ) {
        a(pos) -= 1
        val slice = a.view.slice(pos+1,a.length)
        primero( slice , sum - a(pos) )
        return true
      }
    }
    return false

  }

  private def supersiguiente( a: Array[Int] ) : Int = {
    val sum = a.sum
    if( siguiente( a, sum ) ){
      return sum
    }
    println( "Paso a suma:" + (sum+1) )
    primero(a,sum+1)
    return sum+1
  }

  private def pruebaSimple() {
    val sum = 3
    val s = new Array[Int](3)
    primero(s, sum)
    println(s.mkString(","))
    while (siguiente(s, sum)) {
      println(s.mkString(","))
    }
  }

  def iterator( size: Int, maxSum: Int ) = {
    val s = new Array[Int](size)
    primero(s,1)
    val ret = Iterator.iterate(s.toList){ v =>
      val a = v.toArray
      supersiguiente(a)
      a.toList
    }
    ret.takeWhile( _.sum <= maxSum )
  }
}




class EcuacionMolecularParser extends RegexParsers {

  import EcuacionMolecular._

  def blanco = "\\s*".r

  def atomo: Parser[String] = "[A-Z][a-z]?".r | failure( "Un símbolo atómico es una letra mayúscula con una letra minúscula opcional" )

  def numero: Parser[Int] = "[0-9]+".r ^^ {
    case n => n.toInt
  } | failure( "Se esperaba un número")

  def atomoEnMolecula: Parser[AtomoEnMolecula] = atomo ~ numero.? ^^ {
    case a ~ n => AtomoEnMolecula(a, n.getOrElse(1))
  } | failure( "Se esperaba un símbolo atómico y un número opcional")

  def molecula: Parser[Molecula] = blanco ~> (numero.? ~ rep1(atomoEnMolecula)) <~ blanco ^^ {
    case n ~ as => Molecula(as, n.getOrElse(1))
  } | failure( "Una molécula son varios símbolos atómicos, cada uno con un número opcional")

  def ladoDeEcuacion = molecula ~ rep((blanco ~ "\\+".r ~ blanco) ~> molecula) ^^ {
    case m ~ ms => LadoEcuacion(m :: ms)
  } | failure( "Un lado de la ecuación son varias moléculas separadas por +")

  def ladoIzquierdo = ladoDeEcuacion <~ (blanco ~ ("=".r | "<-*>".r) ) // | failure( "Los lados de la ecuación se separan con = o <-->" )

  def ecuacion = (ladoIzquierdo <~ blanco) ~ ladoDeEcuacion <~ blanco ^^ {
    case li ~ ld => Ecuacion(li, ld)
  }

}

object Probar{
  def testEcuacion = {
    import EcuacionMolecular._
    val ec: Either[String, Option[Ecuacion]] = EcuacionMolecular.parse(" HCl + MnO2 <-->  MnCl2 + H2O + Cl2").map( _.ajusta() )
    println( ec )
  }

  def testSecuencia = {
    import Secuencias._
    for( s <- Secuencias.iterator(3,3) ) println( s.mkString(","))
  }

}
