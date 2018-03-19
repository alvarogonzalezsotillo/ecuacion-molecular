package ecuacion

import ecuacion.EcuacionMolecular.{LadoEcuacion, Molecula}

import scala.reflect.ClassTag
import scala.util.{Success, Try}
import scala.xml._

object Mat{

  trait SolveResult[T]{
    val msg: Elem
    val mat: Mat[T]
    val solved: Boolean = false
  }

  case class Incompatible[T](msg: Elem, mat: Mat[T]) extends SolveResult[T]
  case class SolutionFound[T](msg: Elem, mat: Mat[T], variables: IndexedSeq[T] ) extends SolveResult[T]{
    override val solved = true
  }
  case class VariablesUndefined[T](msg: Elem, mat: Mat[T], variables: IndexedSeq[Option[T]]) extends SolveResult[T]

}

class Mat[T]( values : IndexedSeq[IndexedSeq[T]] )(implicit fractional: Fractional[T], ct: ClassTag[T]){

  type SolveResult = Mat.SolveResult[T]
  type Incompatible = Mat.Incompatible[T]
  type SolutionFound = Mat.SolutionFound[T]
  type VariablesUndefined = Mat.VariablesUndefined[T]

  def this( a : Array[Array[T]] )(implicit f: Fractional[T], ct: ClassTag[T] ){
    this( a.map( _.toIndexedSeq) )
  }

  def addRow( row: IndexedSeq[T] )(implicit fractional: Fractional[T], ct: ClassTag[T]) : Mat[T] = {
    assert( row.size == columns.size )
    new Mat[T]( values ++ IndexedSeq(row) )
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

  def solveUndefined(implicit explicador: Explicador, fractional: Fractional[T] ) : SolveResult = {
    solve match{
      case i : Incompatible =>
        i

      case sf: SolutionFound  =>
        sf

      case Mat.VariablesUndefined(_,diag,_) =>
        // HAY QUE DAR VALOR A LA VARIABLE MÁS PEQUEÑA SI EL TÉRMINO INDEPENDIENTE ES POSITIVO
        import fractional._

        val cero = fractional.fromInt(0)

        val firstUndefinedRow = diag.undefinedRows.head
        explicador.explica(<p>firstUndefinedRow:${firstUndefinedRow.mkString(",")} </p>)
        val row = if( firstUndefinedRow.last > cero ) firstUndefinedRow else firstUndefinedRow.map(f => -f)
        explicador.explica(<p>row:${row.mkString(",")} </p>)

        val coefs = row.take(row.size-1)
        explicador.explica(<p>coefs:${coefs.mkString(",")} </p>)

        val smallerUndefinedIndex = coefs.zipWithIndex.filter{case (c,i) => c != cero }.minBy( _._1)._2 
        explicador.explica(<p>smallerUndefinedIndex:${smallerUndefinedIndex} </p>)

        val newRow = Array.tabulate(columns.size){ i =>
          if( i == smallerUndefinedIndex || i == columns.size-1 ) uno else cero
        }

        explicador.explica(<p>newRow:${newRow} </p>)

        val newMat = diag.addRow(newRow.toIndexedSeq)
        newMat.solveUndefined
    }
  }



  def solve(implicit explicador: Explicador ) : SolveResult  = {
    import fractional.mkNumericOps
    import explicador._

    explica(
      <p>
        El sistema de ecuaciones puede representarse como una matriz y resolverse con una variante del
        <a href="https://es.wikipedia.org/wiki/Eliminaci%C3%B3n_de_Gauss-Jordan">método de Gauss-Jordan</a>
      </p>
    )

    explica( <ecuaciones>{asXML(valuesCopy())}</ecuaciones> )

    explica( <p>La secuencia de combinaciones de filas es la siguiente:</p>)


    val nColumns = columns.size
    val nRows = rows.size

    val diag = diagonalize
    val matDiag = diag.valuesCopy()

    if( matDiag.exists( f => f.take(nColumns-1).forall( _ == cero ) && f.last != cero ) ){
      val error = <p>Hay una fila con coeficientes a cero, pero el término independiente no es cero.</p>
      explica( error )
      return Mat.Incompatible(error,diag)
    }

    def solveVariable(v: Int) : Option[T]= {
      val filaO = diag.rows.find( f => f(v) != cero )
      if( !filaO.isDefined ){
        val error = <p>No se puede encontrar una fila que defina la variable x<sub>{v}</sub></p>
        explica( error )
        return None
      } 
      val fila = filaO.get
      val variablesEnFila = fila.take(nColumns-1).count( _ != cero )
      if( variablesEnFila != 1 ){
        val error = <p>La fila que da valor a la variable x<sub>{v}</sub> no está definida, porque aparece más de una variable</p>
        explica(error)
        return None
      }

      Some(fila(nColumns-1)/fila(v))
    }

    val variables = for( v <- 0 until nColumns-1 ) yield solveVariable(v)
    if( variables.count( _.isDefined) == variables.size ){
      Mat.SolutionFound(<p>Solución encontrada</p>, diag, variables.map(_.get))
    }
    else{
      Mat.VariablesUndefined(<p>Algunas variables no están definidas</p>, diag, variables)
    }
  }

  private def undefinedRows() = {
    for( fila <- rows.take(rows.length-1) ;
      variablesEnFila = fila.take(columns.length-1).count( _ != cero )
      if( variablesEnFila != 1 ) ) yield{
      fila
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
