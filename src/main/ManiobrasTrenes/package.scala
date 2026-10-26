package main

import scala.annotation.tailrec

package object ManiobrasTrenes {
  //t1 es inicial , t2 es el final
  @tailrec
  def buscarVagon(vagonBuscar:Vagon, trenFuente:Tren, iteracion:Int): Int = {
    trenFuente match {
      case Nil => -1
      case x :: xs if x==vagonBuscar => iteracion
      case x :: xs =>  buscarVagon(vagonBuscar, xs, iteracion+1)

    }
  }
  def trenOrganizadoEnteros(t1:Tren , t2:Tren ): Tren = {
    t1 match{
      case Nil => Nil
      case vagon :: xs => buscarVagon(vagon , t2 , 0)+:trenOrganizadoEnteros(xs , t2 )
    }


  }


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
