import main.Derivacion.*

val deEquis = (x :Double) => x
val cuadrado = (x :Double) => x*x
val cubo = (x :Double) => x*x*x
val cte = (x :Double) => 1.5
val quinta = (x :Double) => x*x*x*x*x

derivada(x => x*x)(3)
derivada(x => 7.0)(10)
derivada(x => x*x*x)(-2)


derivadaSuma(cubo, cuadrado)(2)
derivadaSuma(cte, deEquis)(5)
derivadaSuma(cuadrado, cte)(-3)
derivadaSuma(cubo, deEquis)(0)


derivadaResta(cubo, cuadrado)(2)
derivadaResta(cuadrado, cubo)(2)
derivadaResta(deEquis, cte)(7)
derivadaResta(cuadrado, deEquis)(-1)


derivadaMult(deEquis, cuadrado)(3)
derivadaMult(cte, cubo)(2)
derivadaMult(cuadrado, cuadrado)(-2)
derivadaMult(cuadrado, cubo)(1)

derivadaDiv(cuadrado, deEquis)(4)
derivadaDiv(cuadrado, cte)(3)
derivadaDiv(deEquis, cte)(-5)
derivadaDiv(cte, deEquis)(2)