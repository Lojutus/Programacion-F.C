package main

package object ManiobrasTrenes {
  type Vagon = Any
  type Tren = List[Vagon]
  type Estado = (Tren, Tren , Tren)
  trait Movimiento
  case class Uno(n:Int) extends Movimiento
  case class Dos(n:Int) extends Movimiento
  type Maniobra = List[Movimiento]
  def aplicarMovimiento(e:Estado , m:Movimiento ): Estado = {
    val  prueba:Vagon = 0
    val  init:Tren = List(prueba)
    (init , init , init)

  }
  def aplicarMovimientos(e:Estado , movs:Maniobra): List[Estado] = {
    null
  }
  def definirManiobra(t1:Tren , t2:Tren): Maniobra = {
    null
  }

}
