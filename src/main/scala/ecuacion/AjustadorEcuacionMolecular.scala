package ecuacion

import ecuacion.EcuacionMolecular.{LadoEcuacion, Molecula}

import scala.reflect.ClassTag

/**
  * Created by alvaro on 23/12/17.
  */
object AjustadorEcuacionMolecular {

  def log( s: String ) = println(s)

  implicit val explicador = new Explicador {
    override def explica(a : Any) = log(a.toString)
  }
  
  def apply( ecuacion: EcuacionMolecular, maxSum: Int = 30 ): Option[EcuacionMolecular] = {
    //ajustaTanteo(ecuacion,maxSum)
    ajustaAlgebraico(ecuacion)
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

  private def ajustaAlgebraico(e: EcuacionMolecular ) : Option[EcuacionMolecular] = {

    def printM[T]( msg: String, m: Array[Array[T]] ) = {
      log( "*********")
      log( msg )
      for( f <- m ){
        log( f.mkString("\t"))
      }
      log( "*********")
    }

    assert( e.ladoDerecho.atomos().keySet == e.ladoIzquierdo.atomos().keySet )

    implicit val fractional = Racional.FractionalRacional
    import fractional._
    val uno = fractional.fromInt(1)
    val menosuno = fractional.fromInt(-1)
    val cero = fractional.fromInt(0)



    val atomos = e.atomos.keySet.toArray

    log( e.toString )
    log( s"Atomos:${atomos.mkString(",")}")

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

    printM( "Matriz original:" + e.toString, mat )

    val matDiag = new Mat(mat).diagonalize.valuesCopy

    printM( "Matriz diagonalizada:", matDiag )



    val variables = Array.fill[Option[Racional]]( matDiag(0).size )(None)
    variables(0) = Some(uno)

    while( variables.find( _.isEmpty ).isDefined ){
      for( fila <- matDiag ) {
        val a = fila.indexWhere(_ != cero)
        val b = fila.indexWhere(_ != cero, a + 1)

        for( (i1,i2) <- Seq( (a,b), (b,a)) if i1 != -1 && i2 != -1 ){
          if (variables(i1).isDefined && variables(i2).isEmpty) {
            val factor = fila(i1)/fila(i2)
            variables(i2) = variables(i1).map(_ * factor)
          }
        }
      }
    }

    log( "Variables fraccionadas:" + variables.mkString(", "))

    val variablesEnteras = {
      val s = variables.map( _.get )
      val mcm = Racional.mcm(s.map(_.den))
      s.map( r => r.num * mcm / r.den ).map( Math.abs )
    }


    log( "variablesEnteras:" + variablesEnteras.mkString(","))


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

trait Explicador{
  def explica( a : Any )
}

class Mat[T]( values : IndexedSeq[IndexedSeq[T]] )(implicit fractional: Fractional[T], ct: ClassTag[T]){

  def this( a : Array[Array[T]] )(implicit f: Fractional[T], ct: ClassTag[T] ) = this( a.map( _.toIndexedSeq) )

  assert( values.forall( _.size == values(0).size ) )

  val rows = values
  val columns = (0 until rows(0).size ).map{ c =>
    (0 until rows.size).map( r => values(r)(c) )
  }

  def apply(row: Int)(col: Int) = values(row)(col)

  def valuesCopy() = Array.tabulate[T](rows.size,columns.size){ (r, c) =>
    values(r)(c)
  }

  def diagonalize( implicit exp: Explicador ) : Mat[T] = {

    def log( s: String ) = println(s)

    import fractional.mkNumericOps
    val uno = fractional.fromInt(1)
    val menosuno = fractional.fromInt(-1)
    val cero = fractional.fromInt(0)

    val m: Array[Array[T]] = valuesCopy()


    val columns = (m(0).size min m.size)
    log( s"columns:$columns")

    for( col <- 0 until columns ){
      //log( s"diagonalizando columna $col")
      val fil = m.indexWhere{ fila =>
        //log( "Mirando fila:" + fila.mkString((",")) )
        val noEsCero = fila(col) != cero
        val anteriores = fila.take(col)
        //log( "anteriores:" + anteriores.mkString(","))
        val anterioresCero = anteriores.forall( _ == cero )
        //log( s"  noEsCero:$noEsCero  anterioresCero:$anterioresCero" )
        noEsCero && anterioresCero
      }
      //log( s"  col: $col  fil:$fil")
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