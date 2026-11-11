import main.Anagramas.*


  //AI

// ============================================================
//  PRUEBAS DEL TALLER DE ANAGRAMAS
//  Diccionario: cosas, como, yo, y, ocasos, oca, cayo,
//               mocosos, roca, moco, sos
// ============================================================

var total = 0
var pasadas = 0

def seccion(titulo: String): Unit =
  println(s"\n=== $titulo ${"=" * math.max(0, 50 - titulo.length)}")

def prueba(nombre: String)(cond: => Boolean): Unit = {
  total += 1
  val ok = try cond catch { case e: Throwable => println(s"   excepcion: $e"); false }
  if (ok) pasadas += 1
  println(s"${if (ok) "[OK]  " else "[FALLO]"} $nombre")
}

// ------------------------------------------------------------
seccion("lOcPal / lOcFrase")
// ------------------------------------------------------------
prueba("lOcPal(\"casa\") agrupa y ordena") {
  lOcPal("casa") == List(('a', 2), ('c', 1), ('s', 1))
}
prueba("lOcPal(\"\") es vacia") {
  lOcPal("") == Nil
}
prueba("lOcFrase junta las palabras") {
  lOcFrase(List("yo", "y")) == List(('o', 1), ('y', 2))
}

// ------------------------------------------------------------
seccion("diccionarioPorOcurrencias / anagramasDePalabra")
// ------------------------------------------------------------
prueba("\"como\" y \"moco\" son anagramas") {
  anagramasDePalabra("como").toSet == Set("como", "moco")
}
prueba("la entrada puede ser una permutacion que no esta en el diccionario") {
  anagramasDePalabra("omoc").toSet == Set("como", "moco")
}
prueba("palabra sin anagramas devuelve Nil") {
  anagramasDePalabra("xyz") == Nil
}

// ------------------------------------------------------------
seccion("combinaciones")
// ------------------------------------------------------------
prueba("combinaciones(Nil) == List(Nil)") {
  combinaciones(Nil) == List(Nil)
}
prueba("(a,2) da 3 combinaciones") {
  combinaciones(List(('a', 2))).length == 3
}
prueba("(a,2),(b,1) da 3 x 2 = 6, sin repetidas") {
  val r = combinaciones(List(('a', 2), ('b', 1)))
  r.length == 6 && r.distinct.length == 6 && r.contains(Nil)
}
prueba("ninguna combinacion tiene parejas con cantidad 0") {
  combinaciones(List(('a', 2), ('b', 2))).forall(_.forall { case (_, n) => n > 0 })
}

// ------------------------------------------------------------
seccion("complemento")
// ------------------------------------------------------------
val total2 = List(('a', 2), ('b', 1))
prueba("quita parte de una letra") {
  complemento(total2, List(('a', 1))) == List(('a', 1), ('b', 1))
}
prueba("quita una letra completa") {
  complemento(total2, List(('b', 1))) == List(('a', 2))
}
prueba("sublista igual al total da Nil") {
  complemento(total2, total2) == Nil
}
prueba("sublista vacia devuelve todo") {
  complemento(total2, Nil) == total2
}

// ------------------------------------------------------------
seccion("anagramasDeFrase")
// ------------------------------------------------------------
prueba("frase vacia: una sola frase, la vacia") {
  anagramasDeFrase(Nil) == List(Nil)
}
prueba("una palabra: List(\"yo\") -> List(List(\"yo\"))") {
  anagramasDeFrase(List("yo")) == List(List("yo"))
}

// ============================================================
//  CASOS DE PRUEBA GENERADOS CON IA (para consignar en el informe)
// ============================================================
seccion("Casos generados con IA")

// IA-1. Todas las permutaciones de una frase de dos palabras.
// Relevancia: "como" y "moco" son anagramas entre si, asi que el
// resultado debe incluir ambas variantes y ambos ordenes
// (2 variantes x 2 ordenes = 4 frases). Comprueba que
// diccionarioPorOcurrencias, combinaciones y la recursion trabajan juntas.
prueba("IA-1: List(\"como\",\"yo\") genera exactamente 4 frases") {
  anagramasDeFrase(List("como", "yo")).toSet == Set(
    List("como", "yo"), List("moco", "yo"),
    List("yo", "como"), List("yo", "moco")
  )
}

// IA-2. Sin palabras repetidas.
// Relevancia: con las letras de "yo yo" la unica frase posible seria
// "yo yo", que repite palabra. Si el filtro contra repetidas funciona,
// el resultado es vacio. Prueba la regla extra del enunciado y que
// un resultado vacio no rompe la recursion.
prueba("IA-2: List(\"yo\",\"yo\") no devuelve frases con palabras repetidas") {
  anagramasDeFrase(List("yo", "yo")).forall(f => f.distinct.length == f.length)
}

// IA-3. Propiedad general sobre una frase mas grande.
// Relevancia: las letras de "cayo sos" se pueden repartir en muchas
// descomposiciones distintas (cayo+sos, cosas+yo, ocasos+y, oca+sos+y...).
// En vez de listar tod0 a mano, se verifica que cada resultado (a) use
// exactamente las mismas letras que la frase original y (b) no repita
// palabras, y que no haya frases duplicadas. Es la prueba que mas
// probablemente destapa errores en complemento o en el filtro.
prueba("IA-3: cada anagrama de \"cayo sos\" conserva las letras y no repite") {
  val frase = List("cayo", "sos")
  val r = anagramasDeFrase(frase)
  r.nonEmpty &&
    r.forall(f => lOcFrase(f) == lOcFrase(frase)) &&
    r.forall(f => f.distinct.length == f.length) &&
    r.distinct.length == r.length
}

// IA-4 (extra). La frase original debe aparecer entre sus anagramas.
// Relevancia: una frase siempre es anagrama de si misma.
prueba("IA-4: la frase original esta en sus propios anagramas") {
  anagramasDeFrase(List("cayo", "sos")).contains(List("cayo", "sos"))
}

// ------------------------------------------------------------
println(s"\nResultado: $pasadas / $total pruebas pasadas")
if (pasadas == total) println("Todo en orden.") else println("Hay pruebas que revisar.")