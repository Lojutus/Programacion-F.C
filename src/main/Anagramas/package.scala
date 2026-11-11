package main
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

  def lOcFrase(f: Frase): Occurrencias = {
    lOcPal(f.mkString)
  }

  lazy val diccionarioPorOcurrencias: Map[Occurrencias, List[Palabra]] = {
    diccionario.groupBy(lOcPal)
  }

  
  def anagramasDePalabra(pal: Palabra): List[Palabra] =
  {
    val oc : Occurrencias = lOcPal(pal)
    diccionarioPorOcurrencias.getOrElse(oc, Nil)
  }
  def combinaciones(lOcurrencias: Occurrencias): List[Occurrencias] = {
      lOcurrencias match {
        case  Nil => List(Nil)
        case (c , n):: resto =>
          val demas: List[Occurrencias] = combinaciones(resto)
          for{
            i<- (0 to n).toList
            y<- demas
          }yield if (i == 0)  y else (c , i):: y

      }
  }
  //def complemento(lOc: Occurrencias, slOc: Occurrencias): Occurrencias = {}
  //def anagramasDeFrase(frase: Frase): List[Frase] = {}
}