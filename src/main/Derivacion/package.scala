package main
package object Derivacion{
  val h:Double=0.1
  def derivada(f: Double => Double): Double => Double={
    (x0: Double) =>
      (f (x0 - 2*h) - 8 * f (x0 - h) + 8 * f (x0 + h) - f (x0 + 2*h) ) / (12 * h)
  }
  def derivadaSuma(f :Double=>Double , g:Double=>Double):Double => Double ={
    derivada ((x:Double)=> f(x)+g(x))
  }

  def derivadaResta(f: Double => Double, g: Double => Double): Double => Double = {
    derivada((x: Double) => f(x) - g(x))
  }
  def derivadaMult(f: Double => Double, g: Double => Double): Double => Double = {
    derivada((x: Double) => f(x) * g(x))
  }

  def derivadaDiv(f: Double => Double, g: Double => Double): Double => Double = {
    derivada((x: Double) => f(x) / g(x))
  }
}

