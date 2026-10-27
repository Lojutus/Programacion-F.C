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

  def organizarTren(estado:Estado, next:Int): Maniobra = {
    /*
        Casos para tener en cuenta:
        0: Principal y Uno estan vacios; se coloca Dos en principal y se devuelve ese movimento, Finaliar (Hecho)
        1: El numero que sigue esta en la cola; en ese caso se corta y se manda a Dos  , los que esten en Uno vuelven a principal, ReCall (Hecho)
        2: no esta en la cabeza, se corta y se mandan a Uno , ReCall (Hecho)
        En dos se arma el tren
        NOTA IMPORTANTE, POR AHORA LOS MOVIMIENTOS SE LEEN DE DERECHA A IZQUIERDA ( como los arabes )
    */
    estado match {
      case (principal,uno,dos) =>
        principal match { // Caso 0
          case Nil =>
            uno match {
              case Nil =>   List(Dos(-dos.length))
              case y1 =>
                val movimiento1 = Uno(1)
                // Encapsulamiento de los movimientos en una maniobra
                val maniobra: Maniobra = List(movimiento1)
                // Aplicar movimientos para obtener el nuevo estado
                val estadoFinal:Estado = aplicarMovimiento(estado , movimiento1)
                //Concatenacion de la nueva maniobra y nueva llamada para que ordene lo demas
                maniobra ++ organizarTren(estadoFinal, next)
            }
          case cola if cola.last == next => // Caso 1
            // definicion de los movimentos necesarios
              val movimiento1 = Dos(1)
              val movimiento3 = Uno(-uno.length)
          // Encapsulamiento de los movimientos en una maniobra
              val maniobra:Maniobra =  List(movimiento1, movimiento3)
          // Aplicar movimientos para obtener el nuevo estado
              val estadoFinal:Estado = aplicarMovimiento( aplicarMovimiento(estado , movimiento1) , movimiento3)
          //Concatenacion de la nueva maniobra y nueva llamada para que ordene lo demas
              maniobra ++ organizarTren(estadoFinal, next -1)
          case cola => // Caso 2
            val movimiento1 = Uno(1)
            // Encapsulamiento de los movimientos en una maniobra
            val maniobra: Maniobra = List(movimiento1)
            // Aplicar movimientos para obtener el nuevo estado
            val estadoFinal:Estado = aplicarMovimiento(estado , movimiento1)
            //Concatenacion de la nueva maniobra y nueva llamada para que ordene lo demas
            maniobra ++ organizarTren(estadoFinal , next)


        }

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
    val trenNumericoReferencias:Tren = trenOrganizadoEnteros(t1 , t2) // esto develve un tren organizado por referencias, permitiendo organizarlo
    val trenVacio:Tren = Nil;
    organizarTren((trenNumericoReferencias,trenVacio, trenVacio), trenNumericoReferencias.length)
  }

}
