package ecuacion


import ecuacion.EcuacionMolecular.LadoEcuacion

import scala.collection.immutable.IndexedSeq
import scala.util.{Left, Right}
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by alvaro on 12/11/17.
  */

class EcuacionMolecular(val ladoIzquierdo: LadoEcuacion, val ladoDerecho: LadoEcuacion) {

  import EcuacionMolecular._

  override def toString = ladoIzquierdo.toString + " = " + ladoDerecho.toString

  def toHTML = ladoIzquierdo.toHTML + " = " + ladoDerecho.toHTML

  def toXML = <ecuacion>{ladoIzquierdo.toXML} = {ladoDerecho.toXML}</ecuacion>

  def atomos = sumaAtomos( Seq(ladoIzquierdo.atomos(None), ladoDerecho.atomos(None)))

  def esAjustada(multipliers: Option[Seq[Int]] = None ): Boolean = {
    val ni = ladoIzquierdo.moleculas.size
    val nd = ladoDerecho.moleculas.size
    val multipliers_ = multipliers.getOrElse( Iterator.continually(1).take(ni+nd).toSeq )
    assert(ni + nd == multipliers_.size)
    val (mi, md) = multipliers_.splitAt(ni)
    val ai = ladoIzquierdo.atomos(Some(mi))
    val ad = ladoDerecho.atomos(Some(md))
    ai == ad
  }
}

object EcuacionMolecular{

  import scala.xml._
  import scala.xml.NodeSeq._

  trait Grupo{
    val cantidad: Int
    def grupos : Seq[Grupo]
    def atomos( multiplier: Int = 1 ) : Map[String,Int] = sumaAtomos( grupos.map(_.atomos(cantidad*multiplier)) )
    def toHTML: String
    def toXML : Node
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

    override def toXML: Node = <atomo>{elemento}</atomo>
  }

  def toNodeSeq( ns: Seq[NodeSeq] ) : NodeSeq = {
    ns.tail.fold(ns.head){ case (grupo, elem) =>
      grupo ++ elem
    }
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

    override def toXML: Node = {
      val nodes = grupos.map(_.toXML)
      grupos.size match{
        case 1 => <grupo>{nodes.head}<sub>{toStringC(cantidad)}</sub></grupo>
        case _ => <grupo>({toNodeSeq(nodes)})<sub>{toStringC(cantidad)}</sub></grupo>
      }
    }
  }

  case class Molecula( override val grupos:  Seq[Grupo], cantidad: Int = 1 ) extends Grupo{

    override def toString = toStringC(cantidad) + grupos.mkString("")
    override def toHTML = toStringC(cantidad) + grupos.foldLeft(""){case (h,g) => h + g.toHTML}
    override def toXML : Node = {
      val nodes = grupos.map(_.toXML)
      <span>{toStringC(cantidad)}{nodes.tail.foldLeft(Seq(nodes.head)){ case (seq, g) =>
          seq :+ g
      }}</span>
    }
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

    def toXML: Node = {
        val nodes = moleculas.map(_.toXML)
        <span>
          {nodes.tail.foldLeft(Seq(nodes.head)){ case (seq, g) =>
            seq :+ <span>+</span> :+ g
          }}
        </span>
    }

  }


  class EcuacionMolecularParser extends RegexParsers {


    def blanco = "\\s*".r

    def atomo: Parser[Atomo] = "[A-Z][a-z]?".r ^^ {
      case s => Atomo(s)
    }

    def numero: Parser[Int] = "[0-9]+".r ^^ {
      case n => n.toInt
    }



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

    }

    def ladoDeEcuacion : Parser[LadoEcuacion] = molecula ~ rep((blanco ~ "\\+".r ~ blanco) ~> molecula) ^^ {
      case m ~ ms => LadoEcuacion(m :: ms)
    }

    def separadorLados : Parser[Any] = blanco <~ ("=".r | "<-*>".r) ~> blanco


    def ecuacion : Parser[EcuacionMolecular] = (blanco ~> ladoDeEcuacion) ~ separadorLados ~ (ladoDeEcuacion <~ blanco) ^^ {
      case li ~ _ ~ ld => EcuacionMolecular(li, ld)
    }

  }

  def apply( li: LadoEcuacion, ld: LadoEcuacion ) = new EcuacionMolecular(li,ld)

  def apply( s: String ) :  String Either EcuacionMolecular = {
    val parser = new EcuacionMolecularParser
    import parser._
    parser.parse(parser.ecuacion, s) match{
      case Success(ecuacion, input) =>
        if( input.atEnd )
          Right(ecuacion)
        else
          Left("La ecuación no puede entenderse completamente.")
      case NoSuccess(msg,_) =>
        Left(msg)
    }
  }

  val ejemplos = Seq(
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
    "H(O(OH)2)2 + Fe2 = FeH2 + O2",
    "Na2SO4 + BaCl2 = NaCl + BaSO4",
    "FeS + O2 = Fe2O3 + SO2",
    "Al + H2SO4 = Al2(SO4)3 + H2",
    "N2 + H2 = NH3",
    "Na + H2O = NaOH + H2",
    "H2S + O2 = SO2 + H2O",
    "C5H12 + O2 = CO2 + H2O",
    "(NH4)2SO4 + NaOH = Na2SO4 + NH3 + H2O",
    "HCl + MnO2 = Cl2 + MnCl2 + H2O",
    "Na2CO3 + HCl = NaCl + CO2 + H2O",
    "H2 + O2 = H2O",
    "H2SO4 + Al = Al2(SO4)3 + H2",
    "NaCl + H2SO4 = Na2SO4 + HCl",
    "CaCO3 + HCl = CaCl2 + CO2 + H2O",
    "N2 + H2 = NH3",
    "NaClO3 = NaCl + O2",
    "C2 H4 + O2 = CO2 + H2 O",
    "Al2O3 + CO = Al + CO2",
    "C7H16 + O2 = CO2 + H2O",
    "K + H2O = KOH + H2",
    "(NH4)2S + HCl = NH4Cl + H2S",
    "Zn + HNO3 = Zn (NO3)2 + H2",
    "CaC2 + H2O = Ca(OH)2 + C2H2",
    "HCl + Al (OH)3 = Al Cl3 + H2O",
    "H3PO4 + Ca(OH)2 = Ca(H2 PO4)2 + H2O",
    "HCl + MnO2 = MnCl2 + Cl2 + H2O",
    "H2 SO4 + NH4 OH = (NH4)2SO4+ H2O",
    "HCl + Al2O3 = AlCl3 + H2O",
    "C15H32 + O2 = CO2 + H2O",
    "NH3 + O2 = NO + H2O",
    "ZnS + O2 = ZnO + SO2",
    "Fe2O3 + CO = CO2 + Fe",
    "HNO3 + Cu = Cu(NO3)2 + NO2 + H2O",
    "Na3P + H2O = PH3 + NaOH",
    "H3 PO4 + NaOH = Na3PO4 + H2O",
    "KClO3 = KCl + O2",
    "C5H12 + O2 = CO2 + H2O",
    "O2 + Cl2 = Cl2O",
    "C3H8 + O2 = CO2 + H2O",
    "K + H2O = KOH + H2",
    "Cu2O + H2 = Cu + H2O",
    "C9 H20 + O2 = CO2 + H2O",
    "Cu(NO3)2= CuO + NO2 + O2",
    "KOH + H2SO4= K2SO4 + H2O",
    "Na + H2O = NaOH + H2",
    "C6H12  +   O2 =    CO2 +     H2O",
    "C6H12O6 + O2 = CO2 + H2O",
    "C3H8O + O2 = CO2 + H2O",
    "C4H10 + O2 = CO2 + H2O"
  )
}









