package main


package object Multiplicacion {
  def PeasantAlgorithm(x:Int, y:Int ): Int = {
    x
  }

  def PeasantAlgorithmIt(x: Int, y: Int): Int = {
    x
  }

  def splitMultiply(a: Int, b: Int): Int = {
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
      val x = a/ (math.pow(10 , m)).toInt

      val w = b %(math.pow(10 , m)).toInt
      val z = b/(math.pow(10 , m)).toInt
      (((math.pow(10 , m+m).toInt)*(splitMultiply(x,z))) + ((math.pow(10 , m)).toInt * (splitMultiply(y,z) + splitMultiply(x,w))) + splitMultiply(y,w))

    }
  }

  def fastMultiply(x: Int, y: Int): Int = {
    x
  }

}
