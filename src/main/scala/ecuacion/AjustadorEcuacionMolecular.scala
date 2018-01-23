package ecuacion

import ecuacion.EcuacionMolecular.{LadoEcuacion, Molecula}

import scala.reflect.ClassTag
import scala.util.{Success, Try}
import scala.xml._

/**
  * Created by alvaro on 23/12/17.
  */
object AjustadorEcuacionMolecular {

  def log( s: String ) = println(s)

  
  def apply( ecuacion: EcuacionMolecular, maxSum: Int = 30 )(implicit explicador: Explicador): Option[EcuacionMolecular] = {
    //ajustaTanteo(ecuacion,maxSum)
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
      explica( <p>Antes de comenzar, se comprueba que a los dos lados de la ecuación aparecen los mismos átomos. </p> )
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
        explica(<p>Como son iguales, se puede continuar</p>)
      else
        explica(<p>No son iguales, no es posible balancear la ecuación</p>)
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
      val lis = li.map{ case(m,i) => <span>x<sub>{i}</sub>⋅{m.toXML}</span> }
      val base = li.size
      val ld = e.ladoDerecho.moleculas.zipWithIndex
      val lds = ld.map{ case(m,i) => <span>x<sub>{i+base}</sub>⋅{m.toXML}</span> }

      explica( <p>Se asignan variables (x<sub>0</sub>, x<sub>1</sub>, x<sub>2</sub>...) a los coeficientes de cada molécula, de forma que la ecuación molecular pasa a ser:</p> )

      val lisxml = Explicador.intercala( lis, <mas> + </mas> )
      val ldsxml = Explicador.intercala( lds, <mas> + </mas> )

      explica( <ecuaciones><ecuacion>{lisxml} = {ldsxml}</ecuacion></ecuaciones> )
    }

    val mat = {
      def lado( l: LadoEcuacion ) = Array.tabulate( atomos.size, l.moleculas.size ){ (a,m) =>
        val atomo = atomos(a)
        val atomosDeLado = l.moleculas(m).atomos()
        //log( s"Lado:${l.toString} atomosDeLado:$atomosDeLado")
        val coef = atomosDeLado.getOrElse(atomo,0)
        fractional.fromInt(coef)
      }

      def li = lado(e.ladoIzquierdo)
      def ld = lado(e.ladoDerecho)

      li.zip(ld).map{ case (i,d) => i ++ d.map(_ * menosuno)}
    }

    siExplicadorActivo{
      explica( <p>De esta forma se define un sistema de ecuaciones lineales, con una ecuación por cada tipo de átomo</p> )
      explica( <p>El sistema será indeterminado, por lo que se añade la restricción adicional de que  que x<sub>0</sub> tiene valor 1</p> )
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

    val matrizDiagonalizada = new Mat(mat).diagonalize
    val matDiag = matrizDiagonalizada.valuesCopy()


    val variables = matrizDiagonalizada.solveWithoutFreeTerms()


    def explicaVariables[T]( v: Seq[T] ) = {
      val tableBody = for( (valor,i) <- v.zipWithIndex ) yield {
        <tr><td><ecuacion>x<sub>{i}</sub> = {valor}</ecuacion></td></tr>
      }
      explica( <ecuaciones><table>{tableBody}</table></ecuaciones>)

    }

    siExplicadorActivo{
      explica( <p>Tras resolver el sistema, quedan los siguientes valores</p> )
      explicaVariables( variables.map(_.get) )
    }


    val variablesEnteras = {
      val s = variables.map( _.get )
      val mcm = Racional.mcm(s.map(_.den))
      val ret = s.map( r => r.num * mcm / r.den ).map( Math.abs )

      siExplicadorActivo{
        if(s.map(_.den).exists( _ > 1 ) ){
          explica( <p>Algunos valores de variables no son enteros.
            Multiplicaremos cada fracción hasta hacer que todos los denominadores sean el
            mínimo común múltiplo de los originales ({mcm})</p> )
          
          explica( <p>Las variables ajustadas quedan</p> )
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
    log( "ret:" + ret.toString )
    assert( ret.esAjustada() )
    Some(ret)
  }
}


class Mat[T]( values : IndexedSeq[IndexedSeq[T]] )(implicit fractional: Fractional[T], ct: ClassTag[T]){

  def this( a : Array[Array[T]] )(implicit f: Fractional[T], ct: ClassTag[T] ) = this( a.map( _.toIndexedSeq) )

  assert( values.forall( _.size == values(0).size ) )

  val uno = fractional.fromInt(1)
  val menosuno = fractional.fromInt(-1)
  val cero = fractional.fromInt(0)

  val rows = values
  val columns = (0 until rows(0).size ).map{ c =>
    (0 until rows.size).map( r => values(r)(c) )
  }

  def apply(row: Int)(col: Int) = values(row)(col)

  def valuesCopy() = Array.tabulate[T](rows.size,columns.size){ (r, c) =>
    values(r)(c)
  }

  def dump( printline : (String) => Unit = println ) = {
    for( r <- rows ){
      printline( r.mkString("\t") )
    }
  }

  def solve = {
    import fractional.mkNumericOps

    val nColumns = columns.size
    val nRows = rows.size
    assert( nColumns-1 <= nRows, s"El sistema no puede estar definido si hay menos filas ($nRows) que variables ($nColumns-1)" )

    val diag = diagonalize()
    val matDiag = diag.valuesCopy()

    dump()
    diag.dump()

    val variables = for( v <- 0 until nColumns-1 ) yield {
      val filaO = diag.rows.find( f => f(v) != cero )
      assert( filaO.isDefined, s"No se puede encontrar una fila que defina la variable x$v")
      val fila = filaO.get

      val variablesEnFila = fila.take(nColumns-1).count( _ != cero )
      assert( variablesEnFila == 1, s"La fila que da valor a la variable x$v no está definida (depende de variables sin valor $variablesEnFila)" )

      fila(nColumns-1)/fila(v)
    }



    variables
  }

  def solveWithoutFreeTerms(firstVariableHint : T = uno ) = {

    import fractional.mkNumericOps

    val matDiag = valuesCopy()
    val variables = Array.fill[Option[T]]( matDiag(0).size )(None)
    variables(0) = Some(uno)

    var changed = true
    while( changed ){
      changed = false
      for( fila <- matDiag ) {
        val a = fila.indexWhere(_ != cero)
        val b = fila.indexWhere(_ != cero, a + 1)

        for( (i1,i2) <- Seq( (a,b), (b,a)) if i1 != -1 && i2 != -1 ){
          if (variables(i1).isDefined && variables(i2).isEmpty) {
            val factor = fila(i1)/fila(i2)
            //fi1*vi1 + fi2*vi2 = 0   vi2 =  -vi1*fi1/fi2
            variables(i2) = variables(i1).map( -_ * factor)
            changed = true
          }
        }
      }
    }


    variables
  }

  def diagonalize() : Mat[T] = {

    def log( s: String ) = println(s)

    import fractional.mkNumericOps

    val m: Array[Array[T]] = valuesCopy()


    val columns = (m(0).size min m.size)

    for( col <- 0 until columns ){
      val fil = m.indexWhere{ fila =>
        //log( "Mirando fila:" + fila.mkString((",")) )
        val noEsCero = fila(col) != cero
        val anteriores = fila.take(col)
        //log( "anteriores:" + anteriores.mkString(","))
        val anterioresCero = anteriores.forall( _ == cero )
        //log( s"  noEsCero:$noEsCero  anterioresCero:$anterioresCero" )
        noEsCero && anterioresCero
      }

      for( f <- 0 until m.size if f != fil && fil != -1 ){

        val factor = m(f)(col) / m(fil)(col)
        for( c <- col until m(0).size ) {
          m(f)(c) = m(f)(c) - m(fil)(c) * factor
        }
      }


    }
    new Mat(m)

  }

}
