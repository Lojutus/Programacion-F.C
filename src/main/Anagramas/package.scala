package object Anagramas {
  type Palabra = String
  type Frase = List[Palabra]
  type Occurrencias = List[(Char, Int)]

  val diccionario: List[Palabra] = {
    List("cosas", "como", "yo", "y", "ocasos", "oca", "cayo", "mocosos", "roca", "moco", "sos")
  }

  def lOcPal(p: Palabra): Occurrencias = {
    p.groupBy((c: Char) => c)
      .map { case (c, cs) => (c, cs.length) }
      .toList
      .sortBy { case (c, _) => c }
  }

  def lOcFrase(f: Frase): Occurrencias =
    lOcPal(f.mkString)

  //lazy val diccionarioPorOcurrencias: Map[Occurrencias, List[Palabra]] =
  //def anagramasDePalabra(pal: Palabra): List[Palabra] =
  //def combinaciones(lOcurrencias: Occurrencias): List[Occurrencias] ={}
  //def complemento(lOc: Occurrencias, slOc: Occurrencias): Occurrencias = {}
  //def anagramasDeFrase(frase: Frase): List[Frase] = {}
}