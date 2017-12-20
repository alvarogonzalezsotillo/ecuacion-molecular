package ecuacion

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
    def toHTML: String
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
    override val toString = s"$elemento"
    override val toHTML = toString
    override val cantidad = 1
    override val grupos = Seq( GrupoAtomico(Seq(this), cantidad ) )
    override def atomos( multiplier: Int) = Map(elemento -> cantidad*multiplier)
  }

  case class GrupoAtomico(override val grupos:  Seq[Grupo], cantidad: Int = 1) extends Grupo{
    override def toString = grupos.size match {
      case 1 => grupos.head.toString + toStringC(cantidad)
      case _ => "(" + grupos.mkString("") + ")" + toStringC(cantidad)
    }

    override def toHTML = grupos.size match {
      case 1 => grupos.head.toHTML + "<sub>" + toStringC(cantidad) + "</sub>"
      case _ => "(" + grupos.foldLeft(""){case (h,g) => h + g.toHTML} + ")<sub>" + toStringC(cantidad) + "</sub>"
    }

  }

  case class Molecula( override val grupos:  Seq[Grupo], cantidad: Int = 1 ) extends Grupo{
    override def toString = toStringC(cantidad) + grupos.mkString("")
    override def toHTML = toStringC(cantidad) + grupos.foldLeft(""){case (h,g) => h + g.toHTML}
  }


  case class LadoEcuacion(moleculas:  Seq[Molecula]){
    def atomos( multipliers: Option[ Seq[Int]] = None ) : Map[String,Int] = {
      val multipliers_ = multipliers.getOrElse( Iterator.continually(1).take(moleculas.size).toList )
      assert(moleculas.size == multipliers_.size)
      val atomos = moleculas.zip(multipliers_).map{ case (mol,mul) => mol.atomos(mul) }
      sumaAtomos(atomos)
    }
    override def toString = moleculas.mkString(" + ")
    def toHTML = moleculas.tail.foldLeft(moleculas.head.toHTML){ case (h,m) => h + " + " + m.toHTML }
  }

  case class Ecuacion(ladoIzquierdo: LadoEcuacion, ladoDerecho: LadoEcuacion) {


    override def toString = ladoIzquierdo.toString + " = " + ladoDerecho.toString

    def toHTML = ladoIzquierdo.toHTML + " = " + ladoDerecho.toHTML

    def esAjustada(multipliers:  Seq[Int]): Boolean = {
      val ni = ladoIzquierdo.moleculas.size
      val nd = ladoDerecho.moleculas.size
      assert(ni + nd == multipliers.size)
      val (mi, md) = multipliers.splitAt(ni)
      val ai = ladoIzquierdo.atomos(Some(mi))
      val ad = ladoDerecho.atomos(Some(md))
      ai == ad
    }

    def ajusta(maxSum: Int = 30): Option[Ecuacion] = {
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

  class EcuacionMolecularParser extends RegexParsers {


    def blanco = "\\s*".r

    def atomo: Parser[Atomo] = "[A-Z][a-z]?".r ^^ {
      case s => Atomo(s)
    }| failure( "Un símbolo atómico es una letra mayúscula con una letra minúscula opcional" )

    def numero: Parser[Int] = "[0-9]+".r ^^ {
      case n => n.toInt
    } | failure( "Se esperaba un número")



    def grupo : Parser[GrupoAtomico] = rep1(("(" ~> grupo <~ ")"|atomo) ~ numero.?) ~ numero.? ^^ {
      case l ~ c =>

        val grupos = l.map { v =>
          v match {
            case kk: ~[Grupo, Option[Int]] => GrupoAtomico(Seq(kk._1), kk._2.getOrElse(1))
          }
        }

        GrupoAtomico( grupos, c.getOrElse(1))
    }



    def molecula: Parser[Molecula] = blanco ~> (numero.? ~ rep1(grupo)) <~ blanco ^^ {
      case n ~ as if  as.size == 1 && as.head.cantidad == 1 =>
        Molecula( as.head.grupos, n.getOrElse(1))
      case n ~ as =>
        Molecula( as, n.getOrElse(1))

    } | failure( "Una molécula son varios símbolos atómicos, cada uno con un número opcional")

    def ladoDeEcuacion : Parser[LadoEcuacion] = molecula ~ rep((blanco ~ "\\+".r ~ blanco) ~> molecula) ^^ {
      case m ~ ms => LadoEcuacion(m :: ms)
    } | failure( "Un lado de la ecuación son varias moléculas separadas por +")

    def separadorLados : Parser[Any] = blanco <~ ("=".r | "<-*>".r) ~> blanco


    def ecuacion : Parser[Ecuacion] = (blanco ~> ladoDeEcuacion) ~ separadorLados ~ (ladoDeEcuacion <~ blanco) ^^ {
      case li ~ _ ~ ld => Ecuacion(li, ld)
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

  def parseGrupo( s: String ) : String Either Grupo = {
    val parser = new EcuacionMolecularParser
    import parser._
    parser.parse(parser.grupo, s) match{
      case Success(ecuacion, _) => Right(ecuacion)
      case NoSuccess(msg,_) => Left(msg)
    }
  }

  val ejemplos = Seq(
    "H(O(OH)2)2 + Fe2 = FeH2 + O2",
    "H2+ O2 = H2O",
    "N2 +  H2  =   NH3",
    "H2O + Na  = Na(OH) + H2",
    "KClO3 = KCl + O2",
    "BaO2 + HCl = BaCl2 + H2O2",
    "H2SO4 + NaCl =  Na2SO4 + HCl",
    "FeS2 =  Fe3S4 + S2",
    "H2SO4 + C  =  H2O + SO2 + CO2",
    "SO2 + O2 =  SO3",
    "NaCl  = Na + Cl2",
    "HCl + MnO2 =  MnCl2 + H2O + Cl2",
    "K2CO3 + C =  CO + K",
    "Ag2SO4 + NaCl =  Na2SO4 + AgCl",
    "NaNO3 + KCl =  NaCl + KNO3",
    "Fe2O3 + CO =  CO2 + Fe",
    "Na2CO3 + H2O  + CO2 =  NaHCO3",
    "FeS2 + O2 = Fe2O3 + SO2",
    "Cr2O3 + Al =  Al2O3 + Cr",
    "Ag + HNO3 =  NO + H2O + AgNO3",
    "CuFeS2 + O2 =  SO2 + CuO + FeO",
    "Mg + H2SO4 = MgSO4 + H2",
    "C4H10 + O2 = CO2 + H2O",
    "CaCO3 = CaO + CO2",
    "Cd + HCl = CdCl2 + H2",
    "CO + O2 = CO2",
    "MgCO3 = CO2 + MgO",
    "C6H6 + O2 = CO2 + H2O",
    "Al + HCl = AlCl3 + H2",
    "ZnS + O2 = ZnO + SO2",
    "H2O + Na = Na(OH) + H2",
  )
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







