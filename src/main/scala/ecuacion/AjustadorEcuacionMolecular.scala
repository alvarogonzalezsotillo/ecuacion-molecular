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


    if( errorOVariables.isLeft ){
      explica( <p>El sistema no puede resolverse.</p> )
      return None
    }

    val variables = errorOVariables.right.get  

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
            <p>Algunos valores de variables no son enteros.
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


class Mat[T]( values : IndexedSeq[IndexedSeq[T]] )(implicit fractional: Fractional[T], ct: ClassTag[T]){

  println( "HACIENDO UNA MATRIZ:" +values )

  def this( a : Array[Array[T]] )(implicit f: Fractional[T], ct: ClassTag[T] ){
    this( a.map( _.toIndexedSeq) )
  }

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

  def asXML( m: Array[Array[T]]) = {

    val filas = for( f <- m ) yield{
      val fila = f.map( e => <elemento>{e}</elemento> )
      <fila>{fila}</fila>
    }

    <matriz>{filas}</matriz>
  }

  def solve(implicit explicador: Explicador ) : String Either IndexedSeq[T] = {
    import fractional.mkNumericOps
    import explicador._

    explica( <p>El sistema de ecuaciones puede representarse como una matriz y resolverse con una variante del
      <a href="https://es.wikipedia.org/wiki/Eliminaci%C3%B3n_de_Gauss-Jordan">método de Gauss-Jordan</a>
    </p> )

    explica( <ecuaciones>{asXML(valuesCopy())}</ecuaciones> )

    explica( <p>La secuencia de combinaciones de filas es la siguiente:</p>)


    val nColumns = columns.size
    val nRows = rows.size
    if( nColumns-1 > nRows ){
      explica( <p>El sistema no puede estar definido si hay menos filas ({nRows}) que variables ({nColumns-1}) </p> )
    }

    val diag = diagonalize
    val matDiag = diag.valuesCopy()

    if( matDiag.exists( f => f.take(nColumns-1).forall( _ == cero ) && f.last != cero ) ){
      val error = <p>Hay una fila con coeficientes a cero, pero el término independiente no es cero.</p>
      explica( error )
      return Left( error.toString() )
    }

    val erroresOVariables = for( v <- 0 until nColumns-1 ) yield {
      val filaO = diag.rows.find( f => f(v) != cero )
      if( !filaO.isDefined ){
        val error = <p>No se puede encontrar una fila que defina la variable x<sub>{v}</sub></p>
        explica( error )
        return Left(error.toString)
      } 
      val fila = filaO.get
      val variablesEnFila = fila.take(nColumns-1).count( _ != cero )
      if( variablesEnFila != 1 ){
        val error = <p>La fila que da valor a la variable x<sub>{v}</sub> no está definida, porque aparece más de una variable</p>
        explica(error)
        return Left(error.toString)
      }

      Right(fila(nColumns-1)/fila(v))
    }

    val errores = erroresOVariables.filter(_.isLeft).map(_.left)

    if( errores.size > 0 ){
      var error = errores.mkString( ", " )
      return Left(error)
    }
    else{
      val variables = erroresOVariables.map( _.right.get )
      Right(variables)
    }
  }




  def diagonalize(implicit explicador: Explicador ) : Mat[T] = {

    def log( s: String ) = {}

    import fractional.mkNumericOps

    val m: Array[Array[T]] = valuesCopy()


    val columns = (m(0).size min m.size)

    val xml = for( col <- 0 until columns ) yield{
      val fil = m.indexWhere{ fila =>
        val noEsCero = fila(col) != cero
        val anteriores = fila.take(col)
        val anterioresCero = anteriores.forall( _ == cero )
        noEsCero && anterioresCero
      }

      for( f <- 0 until m.size if f != fil && fil != -1 ){

        val factor = m(f)(col) / m(fil)(col)
        for( c <- col until m(0).size ) {
          m(f)(c) = m(f)(c) - m(fil)(c) * factor
        }
      }

      asXML(m)
    }

    explicador.explica( <ecuaciones>{Explicador.intercala( xml, <implica>→</implica> )}</ecuaciones> )


    new Mat(m)

  }

}
