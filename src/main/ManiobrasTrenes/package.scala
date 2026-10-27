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
   def aplicarMovimiento(e: Estado, m: Movimiento): Estado = {
    val (principal, uno, dos) = e
    m match {

      case Uno(n) if n > 0 =>
        val corte          = (principal.length - n) max 0
        val (quedan, van)  = principal.splitAt(corte)
        (quedan, van ++ uno, dos)

      case Uno(n) if n < 0 =>
        val cantidad       = (-n) min uno.length
        val (van, quedan)  = uno.splitAt(cantidad)
        (principal ++ van, quedan, dos)

      case Dos(n) if n > 0 =>
        val corte          = (principal.length - n) max 0
        val (quedan, van)  = principal.splitAt(corte)
        (quedan, uno, van ++ dos)

      case Dos(n) if n < 0 =>
        val cantidad       = (-n) min dos.length
        val (van, quedan)  = dos.splitAt(cantidad)
        (principal ++ van, uno, quedan)

      case _ => e
    }
  }
  def aplicarMovimientos(e:Estado , movs:Maniobra): List[Estado] = {
    null
  }
  def definirManiobra(t1:Tren , t2:Tren): Maniobra = {
    val trenNumericoReferencias:Tren = trenOrganizadoEnteros(t1 , t2)
    null
  }

}
