package main


package object Multiplicacion {
  def PeasantAlgorithm(x:Int, y:Int ): Int = {
    x
  }

  def PeasantAlgorithmIt(x: Int, y: Int): Int = {
    x
  }

  def splitMultiply(a: Long, b: Long): Long = {
    if(a==0 || b ==0){
      return 0
    }
    if(a<10 && b<10){
      a*b
    }
    else{
      val n = (math.log10(a).toInt) +1
      if(!(n%2 == 0) ){

        return (splitMultiply(a*10 , b*10)/100)
      }
      val m = n/2


      val y = a %(math.pow(10 , m)).toInt
      val x = a/(math.pow(10 , m)).toInt

      val w = b %(math.pow(10 , m)).toInt
      val z = b/(math.pow(10 , m)).toInt
      (((math.pow(10 , m+m).toInt)*(splitMultiply(x,z))) + ((math.pow(10 , m)).toInt * (splitMultiply(y,z) + splitMultiply(x,w))) + splitMultiply(y,w))

    }
  }

  def splitMultiply2(a: Long, b: Long): Long = {
    if (a == 0 || b == 0) {
      return 0
    }
    if (a < 10 && b < 10) {
      a * b
    }
    else {
      val na = (math.log10(a).toInt) + 1
      val nb = (math.log10(b).toInt) + 1

      if (na % 2 != 0 && nb % 2 != 0 ) {
        return (splitMultiply2(a * 10, b * 10) / 100)
      }
      else if(!(na % 2 == 0)){
        return splitMultiply2(a * 10, b ) / 10
      }
      else if (!(nb % 2 == 0)) {
        return splitMultiply2(a , b*10) / 10
      }

      val p = (na / 2)
      val r = (nb / 2)


      val y = a % (math.pow(10, p)).toInt
      val x = a / (math.pow(10, p)).toInt

      val w = b % (math.pow(10, r)).toInt
      val z = b / (math.pow(10, r)).toInt
      ((math.pow(10, p + r).toInt) * (splitMultiply2(x, z))) + ((math.pow(10, p)).toInt * (splitMultiply2(x, w)) + (math.pow(10, r)).toInt * splitMultiply2(y, z)) +splitMultiply2(y, w)

    }
  }

  def fastMultiply(a: Long, b: Long): Long = {
    if (a == 0 || b == 0) {
      return 0
    }
    if (a < 10 && b < 10) {
      a * b
    }
    else {
      val na = (math.log10(a).toInt) + 1
      val nb = (math.log10(b).toInt) + 1
      val n =  math.max(na, nb)
      if (n % 2 != 0) {
        return (fastMultiply(a * 10, b * 10) / 100)
      }
      val m = math.max(na, nb)/2



      val y = a % (math.pow(10, m)).toInt
      val x = a / (math.pow(10, m)).toInt

      val w = b % (math.pow(10, m)).toInt
      val z = b / (math.pow(10, m)).toInt

      val z2 = fastMultiply(x, z)
      val z0 = fastMultiply(y, w)
      val z1 = fastMultiply(x + y, z + w) - z2 - z0

      (math.pow(10, 2 * m).toLong * z2) + (math.pow(10, m).toLong * z1) + z0}
  }

}
