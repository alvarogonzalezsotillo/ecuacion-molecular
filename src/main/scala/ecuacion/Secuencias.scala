package ecuacion

object Secuencias {

  import scala.collection.mutable.IndexedSeq


  private def primero(a: IndexedSeq[Int], sum: Int) {
    a(0) = sum
    for (i <- 1 until a.size) a(i) = 0
  }

  private def siguiente(a: Array[Int], sum: Int, pos: Int = 0): Boolean = {
    assert(pos < a.length)
    if (pos < a.length - 1) {
      // INTENTO CAMBIAR LA COLA
      if (siguiente(a, sum - a(pos), pos + 1)) {
        return true
      }

      // SI NO CAMBIA, DEBO BAJAR UNA UNIDAD LA POSICION
      if (a(pos) > 0) {
        a(pos) -= 1
        val slice = a.view.slice(pos + 1, a.length)
        primero(slice, sum - a(pos))
        return true
      }
    }
    return false

  }

  private def supersiguiente(a: Array[Int]): Int = {
    val sum = a.sum
    if (siguiente(a, sum)) {
      return sum
    }
    primero(a, sum + 1)
    return sum + 1
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

  def iterator(size: Int, maxSum: Int) = {
    val s = new Array[Int](size)
    primero(s, 1)
    val ret = Iterator.iterate(s.toList) { v =>
      val a = v.toArray
      supersiguiente(a)
      a.toList
    }
    ret.takeWhile(_.sum <= maxSum)
  }
}
