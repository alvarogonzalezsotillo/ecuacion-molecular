package tutorial.webapp

import java.io.{PrintStream, Serializable, Writer}

import scala.{Option, Serializable}
import scala.collection.mutable.IndexedSeq
import scala.util.{Left, Right}
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by alvaro on 12/11/17.
  */

object EcuacionMolecular{

  trait Grupo{
    val cantidad: Int
    def grupos : Seq[Grupo]
    def atomos( multiplier: Int = 1 ) : Map[String,Int] = sumaAtomos( grupos.map(_.atomos(cantidad*multiplier)) )
  }

  object Grupo{
    def unapply(g: Grupo) = (g.grupos, g.cantidad)
  }

  private def sumaAtomos( l: Seq[Map[String,Int]] ): Map[String, Int] = {
    val keys = l.tail.foldLeft(l.head.keySet)( (s1,s2) => s1 ++ s2.keySet ).toIndexedSeq
    val values = keys.map( k => l.map( _.getOrElse(k,0) ).sum )
    keys.zip(values).toMap
  }

  private def toStringC(i: Int) : String = if( i > 1 ) i.toString else ""

  case class Atomo(elemento: String) extends Grupo{
    override val toString = elemento
    override val cantidad = 1
    override val grupos = Seq( GrupoAtomico(Seq(this), cantidad ) )
    override def atomos( multiplier: Int) = Map(elemento -> cantidad*multiplier)
  }

  case class GrupoAtomico(override val grupos:  Seq[Grupo], cantidad: Int = 1) extends Grupo{
    override def toString = grupos.size match {
      case 1 => grupos.head.toString + toStringC(cantidad)
      case _ => "(" + grupos.mkString("") + ")" + toStringC(cantidad)
    }
  }

  case class Molecula( override val grupos:  Seq[Grupo], cantidad: Int = 1 ) extends Grupo{
    override def toString = toStringC(cantidad) + grupos.mkString("")
  }


  case class LadoEcuacion(moleculas:  Seq[Molecula]){
    def atomos( multipliers: Option[ Seq[Int]] = None ) : Map[String,Int] = {
      val multipliers_ = multipliers.getOrElse( Iterator.continually(1).take(moleculas.size).toList )
      assert(moleculas.size == multipliers_.size)
      val atomos = moleculas.zip(multipliers_).map{ case (mol,mul) => mol.atomos(mul) }
      sumaAtomos(atomos)
    }
    override def toString = moleculas.mkString(" + ")
  }

  case class Ecuacion(ladoIzquierdo: LadoEcuacion, ladoDerecho: LadoEcuacion) {


    override def toString: String = {
      ladoIzquierdo.toString + " = " + ladoDerecho.toString
    }

    def esAjustada(multipliers:  Seq[Int]): Boolean = {
      val ni = ladoIzquierdo.moleculas.size
      val nd = ladoDerecho.moleculas.size
      assert(ni + nd == multipliers.size)
      val (mi, md) = multipliers.splitAt(ni)
      val ai = ladoIzquierdo.atomos(Some(mi))
      val ad = ladoDerecho.atomos(Some(md))
      ai == ad
    }

    def ajusta(maxSum: Int = 100): Option[Ecuacion] = {
      val it_ = Secuencias.iterator(ladoDerecho.moleculas.size + ladoIzquierdo.moleculas.size, maxSum)
      val it = it_.map { l => l.map(_ + 1) }
      val multipliers = it.find(esAjustada(_))


      multipliers.map { m =>
        val (mi, md) = m.splitAt(ladoIzquierdo.moleculas.size)
        val li = ladoIzquierdo.moleculas.zip(mi).map { case (mol, mul) =>
          Molecula(mol.grupos, mol.cantidad * mul)
        }
        val ld = ladoDerecho.moleculas.zip(md).map { case (mol, mul) =>
          Molecula(mol.grupos, mol.cantidad * mul)
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

  def atomo: Parser[Atomo] = "[A-Z][a-z]?".r ^^ {
    case s => Atomo(s)
  }| failure( "Un símbolo atómico es una letra mayúscula con una letra minúscula opcional" )

  def numero: Parser[Int] = "[0-9]+".r ^^ {
    case n => n.toInt
  } | failure( "Se esperaba un número")



  def grupo : Parser[GrupoAtomico] = (atomo|("\\(".r ~> grupo <~ "\\)".r)) ~ numero.? ^^ {
    case g ~ c => g match{
      case a : Atomo => GrupoAtomico(Seq(a), c.getOrElse(1))
      case g : GrupoAtomico => GrupoAtomico( g.grupos, c.getOrElse(1) )
    }
  }


  def molecula: Parser[Molecula] = blanco ~> (numero.? ~ rep1(grupo)) <~ blanco ^^ {
    case n ~ as => Molecula( as, n.getOrElse(1))
  } | failure( "Una molécula son varios símbolos atómicos, cada uno con un número opcional")

  def ladoDeEcuacion = molecula ~ rep((blanco ~ "\\+".r ~ blanco) ~> molecula) ^^ {
    case m ~ ms => LadoEcuacion(m :: ms)
  } | failure( "Un lado de la ecuación son varias moléculas separadas por +")

  def ladoIzquierdo = ladoDeEcuacion <~ (blanco ~ ("=".r | "<-*>".r) ) // | failure( "Los lados de la ecuación se separan con = o <-->" )

  def ecuacion = (ladoIzquierdo <~ blanco) ~ ladoDeEcuacion <~ blanco ^^ {
    case li ~ ld => Ecuacion(li, ld)
  }

}

object Probar extends App{
  def testEcuacion = {
    import EcuacionMolecular._
    val ecuacion = EcuacionMolecular.parse(" HCl + MnO2 <-->  MnCl2 + H2O + Cl2")
    println( ecuacion )
    val ecuacionAjustada: Either[String, Option[Ecuacion]] = ecuacion.map( _.ajusta() )
    println( ecuacionAjustada )
  }

  def testSecuencia = {
    import Secuencias._
    for( s <- Secuencias.iterator(3,3) ) println( s.mkString(","))
  }

  testEcuacion
}
