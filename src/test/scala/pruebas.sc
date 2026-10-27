import main.ManiobrasTrenes.*
def mostrarEstado(estado: Estado): Unit = {
  println(s"  Principal: ${estado._1}")
  println(s"  Uno:      ${estado._2}")
  println(s"  Dos:      ${estado._3}")
}

def probarOrganizar(nombre: String, inicial: Estado , tamaño:Int): Unit = {
  println()
  println("=" * 55)
  println(s"PRUEBA: $nombre")
  println("=" * 55)

  println("Estado inicial:")
  mostrarEstado(inicial)

  try {
    val maniobra = organizarTren(inicial , tamaño)

    println()
    println(s"Maniobra generada: ${maniobra.mkString(" -> ")}")


    var estadoActual = inicial

    maniobra.zipWithIndex.foreach {
      case (movimiento, indice) =>
        println()
        println(s"Paso ${indice + 1}: $movimiento")

        estadoActual = aplicarMovimiento(estadoActual, movimiento)

        mostrarEstado(estadoActual)
    }

    println()
    println("Estado final obtenido:")
    mostrarEstado(estadoActual)

  } catch {
    case _: StackOverflowError =>
      println("ERROR: posible recursión infinita o excesivamente profunda.")

    case e: Exception =>
      println(s"ERROR: ${e.getClass.getSimpleName}: ${e.getMessage}")
  }
}

// Caso 1: tren vacío
probarOrganizar(
  "Tren completamente vacío",
  (Nil, Nil, Nil),
  0
)

// Caso 2: un solo vagón
probarOrganizar(
  "Un solo vagón",
  (List(1), Nil, Nil),
  1
)

// Caso 3: tren ya ordenado
probarOrganizar(
  "Tren ordenado",
  (List(1, 2, 3), Nil, Nil),
  3
)

// Caso 4: tren en orden inverso
probarOrganizar(
  "Tren en orden inverso",
  (List(3, 2, 1), Nil, Nil),
  3
)

// Caso 5: tren desordenado
probarOrganizar(
  "Tren desordenado A",
  (List(3, 1, 2), Nil, Nil),
  3
)

// Caso 6: otra distribución
probarOrganizar(
  "Tren desordenado B",
  (List(2, 3, 1), Nil, Nil),
  3
)

// Caso 7: incluye el cero
probarOrganizar(
  "Tren que contiene el cero ( deberia de dar error)",
  (List(2, 0, 1), Nil, Nil),
  3
)

// Caso 8: dos vagones
probarOrganizar(
  "Dos vagones invertidos",
  (List(2, 1), Nil, Nil),
  2
)
// Caso propio 1: principal y Uno vacio
probarOrganizar(
  "principal y Uno vacio ",
  (Nil, Nil, List(1 ,2 , 3)),
  0
)
// Caso propio 2: principal listo para solucionar tamaño 1
probarOrganizar(
  " principal listo para solucionar tamaño 1 ",
  (List(1), Nil, Nil),
  1
)