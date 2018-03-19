package ecuacion

import ecuacion.EcuacionMolecular.{LadoEcuacion, Molecula}

import scala.reflect.ClassTag
import scala.util.{Success, Try}
import scala.xml._
import ecuacion.Mat._

/**
  * Created by alvaro on 23/12/17.
  */
object AjustadorEcuacionMolecular {

  def log( s: String ) = println(s)

  
  def apply( ecuacion: EcuacionMolecular, maxSum: Int = 30 )(implicit explicador: Explicador): Option[EcuacionMolecular] = {
    Try(ajustaAlgebraico(ecuacion)) match{
      case Success(Some(ec)) => Some(ec)
      case _ => None
    }
  }

  private def ajustaTanteo(e: EcuacionMolecular, maxSum: Int = 30): Option[EcuacionMolecular] = {

    assert( e.ladoDerecho.atomos().keySet == e.ladoIzquierdo.atomos().keySet )

    val it_ = Secuencias.iterator(e.ladoDerecho.moleculas.size + e.ladoIzquierdo.moleculas.size, maxSum)
    val it = it_.map { l => l.map(_ + 1) }
    val multipliers = it.find( m => e.esAjustada(Some(m)))


    multipliers.map { m =>
      val (mi, md) = m.splitAt(e.ladoIzquierdo.moleculas.size)
      val li = e.ladoIzquierdo.moleculas.zip(mi).map { case (mol, mul) =>
        Molecula(mol.grupos, mol.cantidad * mul)
      }
      val ld = e.ladoDerecho.moleculas.zip(md).map { case (mol, mul) =>
        Molecula(mol.grupos, mol.cantidad * mul)
      }
      EcuacionMolecular(LadoEcuacion(li), LadoEcuacion(ld))
    }
  }

  private def ajustaAlgebraico(e: EcuacionMolecular )(implicit explicador: Explicador) : Option[EcuacionMolecular] = {

    import explicador.siExplicadorActivo
    import explicador.explica



    val atomosLadoDerecho = e.ladoDerecho.atomos().keySet
    val atomosLadoIzquierdo = e.ladoIzquierdo.atomos().keySet

    siExplicadorActivo {
      explica( <p>Antes de comenzar, se comprueba que a los dos lados de la ecuación aparecen los mismos átomos: </p> )
      explica(
          <ul>
            <li>
              Átomos en la derecha:
              {atomosLadoDerecho.toSeq.sorted.mkString(",")}
            </li>
            <li>
              Átomos en la izquierda:
              {atomosLadoIzquierdo.toSeq.sorted.mkString(",")}
            </li>
          </ul>
      )
      if (atomosLadoDerecho == atomosLadoIzquierdo)
        explica(<p>Como son iguales, se puede continuar.</p>)
      else
        explica(<p>No son iguales, no es posible balancear la ecuación.</p>)
    }


    assert( atomosLadoDerecho == atomosLadoIzquierdo )

    implicit val fractional = Racional.FractionalRacional
    import fractional._
    val cero = fractional.fromInt(0)
    val uno = fractional.fromInt(1)
    val menosuno = fractional.fromInt(-1)

    val atomos = e.atomos.keySet.toArray


    siExplicadorActivo{
      val li = e.ladoIzquierdo.moleculas.zipWithIndex
      val lis = li.map{ case(m,i) => <span><b>x<sub>{i}</sub></b>⋅{m.toXML}</span> }
      val base = li.size
      val ld = e.ladoDerecho.moleculas.zipWithIndex
      val lds = ld.map{ case(m,i) => <span><b>x<sub>{i+base}</sub></b>⋅{m.toXML}</span> }

      explica( <p>Se asignan variables (x<sub>0</sub>, x<sub>1</sub>, x<sub>2</sub>...) a los coeficientes de cada molécula, de forma que la ecuación molecular pasa a ser:</p> )

      val lisxml = Explicador.intercala( lis, <mas> + </mas> )
      val ldsxml = Explicador.intercala( lds, <mas> + </mas> )

      explica( <ecuaciones><ecuacion>{lisxml} = {ldsxml}</ecuacion></ecuaciones> )
    }

    val mat = {
      def lado( l: LadoEcuacion ) = Array.tabulate( atomos.size, l.moleculas.size ){ (a,m) =>
        val atomo = atomos(a)
        val atomosDeLado = l.moleculas(m).atomos()
        val coef = atomosDeLado.getOrElse(atomo,0)
        fractional.fromInt(coef)
      }

      def li = lado(e.ladoIzquierdo)
      def ld = lado(e.ladoDerecho)

      li.zip(ld).map{ case (i,d) => i ++ d.map(_ * menosuno)}
    }

    siExplicadorActivo{
      explica(
        <p>
          De esta forma se define un sistema de ecuaciones lineales, con una ecuación por cada tipo de átomo.
          El sistema será <a href="https://es.wikipedia.org/wiki/Sistema_de_ecuaciones_algebraicas#Clasificaci%C3%B3n_de_los_sistemas">indeterminado</a>,
          por lo que se añade la restricción adicional de que  que x<sub>0</sub> tiene valor 1.
        </p>
      )
      explica( <p>Las ecuaciones resultantes serían</p> )
      val tableBody = for( (a,fila) <- atomos.zip(mat) ) yield {
        val ec = fila.zipWithIndex.map{ case(coef,n) =>
          val signo = if( coef > cero ) <mas> + </mas> else <menos> - </menos>
          val ret = coef match{
            case x if x == cero => <coef></coef>
            case x if (x == uno || x == menosuno) => <coef>{signo}x<sub>{n}</sub></coef>
            case _ => <coef>{signo}{Math.abs(coef.toInt)}⋅x<sub>{n}</sub></coef>
          }
          ret
        }
        <tr><td>{a}</td><td><ecuacion>{ec} = 0</ecuacion></td></tr>
      }
      explica(
        <ecuaciones>
          <table>
            <tr><th>Átomo</th><th>Ecuación</th></tr>
            {tableBody}
            <tr><td><i>Restricción adicional</i></td><td><mas> + </mas>x<sub>0</sub> = 1 </td></tr>
          </table>
        </ecuaciones>
      )
    }

    val adicional = Array(uno) ++ Array.fill(mat(0).size-1)(cero) :+ uno
    val toSolve = mat.map( array => array :+ cero ) :+ adicional
    val matriz = new Mat(toSolve)

    val errorOVariables = matriz.solve

    println( s"errorOvariables:$errorOVariables" )


    if( !errorOVariables.solved ){
      explica( <p>El sistema no puede resolverse.</p> )
      return None
    }

    val variables = errorOVariables.asInstanceOf[SolutionFound[Racional]].variables

    def explicaVariables[T]( v: Seq[T] ) = {
      val tableBody = for( (valor,i) <- v.zipWithIndex ) yield {
        <tr><td><ecuacion>x<sub>{i}</sub> = {valor}</ecuacion></td></tr>
      }
      explica( <ecuaciones><table>{tableBody}</table></ecuaciones>)
    }

    siExplicadorActivo{
      explica( <p>Tras resolver el sistema, quedan los siguientes valores:</p> )
      explicaVariables( variables )
    }

    if( variables.exists( _ == cero ) ){
      explica( <p>Alguna variable ha quedado a cero, lo que indica que la ecuación no puede ajustarse.</p> )
      return None
    }


    val variablesEnteras = {
      val denominadores = variables.map(_.den)
      val mcm = Racional.mcm(denominadores)
      val ret = variables.map( r => r.num * mcm / r.den ).map( Math.abs )

      siExplicadorActivo{
        if(denominadores.exists( _ > 1 ) ){
          explica(
            <p>
              Algunos valores de variables no son enteros.
              Multiplicaremos cada fracción hasta hacer que todos los denominadores sean el
              mínimo común múltiplo de los originales.
            </p>
          )
          explica(
            <ecuaciones>
              <ecuacion>
                mcm({denominadores.mkString(",")}) = {mcm}
              </ecuacion>
            </ecuaciones>
          )
          
          explica( <p>Las variables ajustadas quedan:</p> )
          explicaVariables( ret )
        }

      }

      ret
    }



    val (mi, md) = variablesEnteras.splitAt(e.ladoIzquierdo.moleculas.size)
    val li = e.ladoIzquierdo.moleculas.zip(mi).map { case (mol, mul) =>
      Molecula(mol.grupos, mol.cantidad * mul)
    }
    val ld = e.ladoDerecho.moleculas.zip(md).map { case (mol, mul) =>
      Molecula(mol.grupos, mol.cantidad * mul)
    }

    val ret = EcuacionMolecular(LadoEcuacion(li), LadoEcuacion(ld))

    explica(
      <p>La ecuación {ret.toXML} queda ajustada.</p>
    )

    Some(ret)
  }
}



