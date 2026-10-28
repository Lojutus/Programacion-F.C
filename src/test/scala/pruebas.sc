import main.ManiobrasTrenes.*

//PRUEBAS AI
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
var totalPruebasVagon = 0
var fallidasPruebasVagon = 0

def registrarResultado(
                        nombre: String,
                        esperado: Any,
                        obtenido: Any
                      ): Unit = {
  totalPruebasVagon += 1

  if (obtenido == esperado) {
    println(s"[OK] $nombre")
  } else {
    fallidasPruebasVagon += 1
    println(s"[FALLO] $nombre")
  }

  println(s"  Esperado: $esperado")
  println(s"  Obtenido: $obtenido")
  println("-" * 45)
}

def probarBuscarVagon(
                       nombre: String,
                       buscado: Vagon,
                       fuente: Tren,
                       inicio: Int,
                       esperado: Int
                     ): Unit = {
  println()
  println(s"BUSCAR: $nombre")
  println(s"  Vagón buscado: $buscado")
  println(s"  Tren fuente: $fuente")
  println(s"  Iteración inicial: $inicio")

  val obtenido = buscarVagon(buscado, fuente, inicio)

  registrarResultado(nombre, esperado, obtenido)
}

def probarTrenOrganizado(
                          nombre: String,
                          t1: Tren,
                          t2: Tren,
                          esperado: Tren
                        ): Unit = {
  println()
  println("=" * 55)
  println(s"ORGANIZAR: $nombre")
  println("=" * 55)
  println(s"Tren 1: $t1")
  println(s"Tren 2: $t2")

  println("Referencia de cada vagón de t1 dentro de t2:")

  t1.zipWithIndex.foreach {
    case (vagon, indice) =>
      val referencia = buscarVagon(vagon, t2, 0)
      println(s"  t1[$indice] = $vagon -> posición $referencia")
  }

  val obtenido = trenOrganizadoEnteros(t1, t2)

  registrarResultado(nombre, esperado, obtenido)
}
probarBuscarVagon(
  "Buscar en un tren vacío",
  "A", Nil, 0, -1
)

probarBuscarVagon(
  "Encontrar el único vagón",
  "A", List("A"), 0, 0
)

probarBuscarVagon(
  "Encontrar el primer vagón",
  "A", List("A", "B", "C"), 0, 0
)

probarBuscarVagon(
  "Encontrar un vagón intermedio",
  "B", List("A", "B", "C"), 0, 1
)

probarBuscarVagon(
  "Encontrar el último vagón",
  "D", List("A", "B", "C", "D"), 0, 3
)

probarBuscarVagon(
  "Vagón que no existe",
  "Z", List("A", "B", "C"), 0, -1
)

probarBuscarVagon(
  "Vagón repetido: debe devolver la primera aparición",
  "A", List("B", "A", "C", "A"), 0, 1
)

probarBuscarVagon(
  "La posición inicial no es cero",
  "B", List("A", "B", "C"), 10, 11
)
probarBuscarVagon(
  "Buscar un entero",
  30, List(10, 20, 30, 40), 0, 2
)

probarBuscarVagon(
  "Buscar un Boolean",
  true, List(false, false, true), 0, 2
)

probarBuscarVagon(
  "Buscar un carácter",
  'z', List('x', 'y', 'z'), 0, 2
)

probarBuscarVagon(
  "Buscar un decimal",
  3.14, List(2.71, 1.41, 3.14), 0, 2
)

probarBuscarVagon(
  "Buscar una tupla",
  ("Cali", 76),
  List(("Bogota", 1), ("Cali", 76)),
  0, 1
)

probarBuscarVagon(
  "Buscar una lista como vagón ",
  List(2, 3),
  List(List(9), List(2, 3), List(4)),
  0, 1
)

probarBuscarVagon(
  "Distinguir el número 42 del texto 42",
  "42", List(42, "42", true), 0, 1
)
def mostrarEstadoManiobra(e: Estado): Unit = {
  println(s"  Principal: ${e._1}")
  println(s"  Uno:       ${e._2}")
  println(s"  Dos:       ${e._3}")
}

def probarDefinirManiobra(
                           nombre: String,
                           t1: Tren,
                           t2: Tren
                         ): Unit = {
  println()
  println("=" * 60)
  println(s"PRUEBA: $nombre")
  println("=" * 60)

  println(s"Tren inicial: $t1")
  println(s"Tren esperado: $t2")

  try {
    val referencias = trenOrganizadoEnteros(t1, t2)
    println(s"Referencias calculadas : $referencias")

    val maniobra = definirManiobra(t1, t2)
    println(s"Maniobra generada: ${maniobra.mkString(" -> ")}")

    var estadoActual: Estado = (t1, Nil, Nil)

    println()
    println("Estado inicial:")
    mostrarEstadoManiobra(estadoActual)

    maniobra.zipWithIndex.foreach {
      case (movimiento, indice) =>
        println()
        println(s"Paso ${indice + 1}: $movimiento")

        estadoActual = aplicarMovimiento(estadoActual, movimiento)

        mostrarEstadoManiobra(estadoActual)
    }

    println()
    println(s"Tren final: ${estadoActual._1}")
    println(s"Tren esperado: $t2")

    val correcto =
      estadoActual._1 == t2 &&
        estadoActual._2.isEmpty &&
        estadoActual._3.isEmpty

    if (correcto) {
      println("[OK] Se alcanzó el estado esperado.")
    } else {
      println("[REVISAR] El estado final no coincide con el esperado.")
    }

  } catch {
    case _: StackOverflowError =>
      println("[ERROR] La ejecución produjo un StackOverflowError.")

    case e: Exception =>
      println(s"[ERROR] ${e.getClass.getSimpleName}: ${e.getMessage}")
  }
}
probarDefinirManiobra(
  "Trenes vacíos",
  Nil,
  Nil
)

probarDefinirManiobra(
  "Un solo vagón",
  List(1),
  List(1)
)

probarDefinirManiobra(
  "Tren ya ordenado",
  List(1, 2, 3),
  List(1, 2, 3)
)

probarDefinirManiobra(
  "Tren en orden inverso",
  List(1, 2, 3),
  List(3, 2, 1)
)

probarDefinirManiobra(
  "Rotación de tres vagones",
  List(3, 1, 2),
  List(1, 2, 3)
)

probarDefinirManiobra(
  "Desorden de cuatro vagones",
  List(2, 4, 1, 3),
  List(4, 1, 3, 2)
)

probarDefinirManiobra(
  "Desorden de cinco vagones",
  List(5, 2, 4, 1, 3),
  List(3, 1, 5, 2, 4)
)
probarDefinirManiobra(
  "Cadenas de texto",
  List("Cali", "Palmira", "Bogotá"),
  List("Palmira", "Bogotá", "Cali")
)


probarDefinirManiobra(
  "Tipos mezclados",
  List("hola", 42, true, 'x'),
  List(true, 'x', "hola", 42)
)

probarDefinirManiobra(
  "Tuplas como vagones",
  List(("Cali", 1), ("Palmira", 2), ("Bogotá", 3)),
  List(("Bogotá", 3), ("Cali", 1), ("Palmira", 2))
)

probarDefinirManiobra(
  "Listas como vagones",
  List(List(1, 2), List(3, 4), List(5, 6)),
  List(List(5, 6), List(1, 2), List(3, 4))
)