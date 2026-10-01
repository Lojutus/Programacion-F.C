package main

package object MultiplicacionAi {
  def PeasantAlgorithm(a: Int, b: Int): Int = {
    if (a == 0) 0
    else if (a % 2 == 0) PeasantAlgorithm(a / 2, b + b)
    else b + PeasantAlgorithm(a / 2, b + b)
  }

  def PeasantAlgorithmIt(x: Int, y: Int): Int = {
    @scala.annotation.tailrec
    def iter(a: Int, b: Int, acc: Int): Int = {
      if (a == 0) acc
      else if (a % 2 == 0) iter(a / 2, b + b, acc)
      else iter(a / 2, b + b, acc + b)
    }

    iter(x, y, 0)
  }
  def numDigitos(x: Int): Int =
    if (x < 10) 1 else 1 + numDigitos(x / 10)

  def splitMultiply(a: Int, b: Int): Int = {
    if (a < 10 && b < 10) a * b
    else {
      val n = math.max(numDigitos(a), numDigitos(b))
      val m = n / 2
      val p = math.pow(10, m).toInt

      val a1 = a / p
      val a0 = a % p
      val b1 = b / p
      val b0 = b % p

      val z2 = splitMultiply(a1, b1)
      val z1 = splitMultiply(a1, b0) + splitMultiply(a0, b1)
      val z0 = splitMultiply(a0, b0)

      z2 * (p * p) + z1 * p + z0
    }
  }

  def fastMultiply(a: Int, b: Int): Int = {
    if (a < 10 && b < 10) a * b
    else {
      val n = math.max(numDigitos(a), numDigitos(b))
      val m = n / 2
      val p = math.pow(10, m).toInt

      val a1 = a / p
      val a0 = a % p
      val b1 = b / p
      val b0 = b % p

      val z2 = fastMultiply(a1, b1)
      val z0 = fastMultiply(a0, b0)
      val z1 = fastMultiply(a1 + a0, b1 + b0) - z2 - z0

      z2 * (p * p) + z1 * p + z0
    }
  }
}
