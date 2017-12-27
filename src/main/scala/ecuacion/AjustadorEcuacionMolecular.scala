package ecuacion

import ecuacion.EcuacionMolecular.{LadoEcuacion, Molecula}

import scala.reflect.ClassTag

/**
  * Created by alvaro on 23/12/17.
  */
object AjustadorEcuacionMolecular {

  def apply[T: ClassTag]( ecuacion: EcuacionMolecular, maxSum: Int = 30 )(implicit dummy: Fractional[T]): Option[EcuacionMolecular] = {
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

  private def ajustaAlgebraico[T: ClassTag](e: EcuacionMolecular )(implicit fractional: Fractional[T]) : Option[EcuacionMolecular] = {

    def printM( msg: String, m: Array[Array[T]] ) = {
      println( "*********")
      println( msg )
      for( f <- m ){
        println( f.mkString("\t"))
      }
      println( "*********")
    }

    assert( e.ladoDerecho.atomos().keySet == e.ladoIzquierdo.atomos().keySet )

    import fractional._
    val uno = fractional.fromInt(1)
    val menosuno = fractional.fromInt(-1)
    val cero = fractional.fromInt(0)



    val atomos = e.atomos.keySet.toArray

    println( e.toString )
    println( s"Atomos:${atomos.mkString(",")}")

    val mat = {
      def lado( l: LadoEcuacion ) = Array.tabulate[T]( atomos.size, l.moleculas.size ){ (a,m) =>
        val atomo = atomos(a)
        val atomosDeLado = l.moleculas(m).atomos()
        println( s"Lado:${l.toString} atomosDeLado:$atomosDeLado")
        val coef = atomosDeLado.getOrElse(atomo,0)
        fractional.fromInt(coef)
      }

      def li = lado(e.ladoIzquierdo)
      def ld = lado(e.ladoDerecho)

      li.zip(ld).map{ case (i,d) => i ++ d.map(_ * menosuno)}
    }

    printM( "Matriz original:" + e.toString, mat )

    def diagonaliza( m: Array[Array[T]]): Unit ={

      import fractional.mkNumericOps

      for( col <- 0 until (m(0).size min m.size) ){
        val fil = m.indexWhere( fila => fila(col) != cero )
        assert( fil != -1 )
        for( f <- 0 until m.size if f != fil ){

          val factor = m(f)(col) / m(fil)(col)
          for( c <- col until m(0).size ) {
            m(f)(c) = m(f)(c) - m(fil)(c) * factor
          }
        }
      }
    }

    diagonaliza(mat)
    printM( "Matriz diagonalizada:", mat )


    val variables: Array[Option[T]] = Array.fill[Option[T]]( mat(0).size )(None)
    variables(0) = Some(uno)

    while( variables.find( _.isEmpty ).isDefined ){
      for( fila <- mat ) {
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

    println( "Variables:" + variables.mkString(", "))

    val m = variables.map( _.get ).map( _.toInt() ).map(Math.abs)

    println( "m:" + m.mkString(","))


    val (mi, md) = m.splitAt(e.ladoIzquierdo.moleculas.size)
    val li = e.ladoIzquierdo.moleculas.zip(mi).map { case (mol, mul) =>
      Molecula(mol.grupos, mol.cantidad * mul)
    }
    val ld = e.ladoDerecho.moleculas.zip(md).map { case (mol, mul) =>
      Molecula(mol.grupos, mol.cantidad * mul)
    }

    val ret = EcuacionMolecular(LadoEcuacion(li), LadoEcuacion(ld))
    println( "ret:" + ret.toString )
    assert( ret.esAjustada() )
    Some(ret)
  }
}
