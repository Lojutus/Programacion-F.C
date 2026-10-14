import main.Derivacion.*

val deEquis = (x :Double) => x
val cuadrado = (x :Double) => x*x
val cubo = (x :Double) => x*x*x
val cte = (x :Double) => 1.5
val quinta = (x :Double) => x*x*x*x*x

derivada(deEquis)(2)
derivada(cuadrado)(8)
derivada(quinta)(10)
derivada(cubo)(20)
derivada(cte)(100)

derivadaSuma(cte,cuadrado)(4)
derivadaSuma(cubo,cuadrado)(10)
derivadaSuma(cubo,quinta)(8)
derivadaSuma(quinta,cte)(10)
derivadaSuma(deEquis,cuadrado)(80)

derivadaResta(cuadrado,cubo)(20)
derivadaResta(cubo,quinta)(30)
derivadaResta(deEquis,cuadrado)(100)
derivadaResta(cuadrado,cuadrado)(200)
derivadaResta(quinta,cte)(-35)

derivadaMult(cubo,cte)(-40)
derivadaMult(cte,cuadrado)(0)
derivadaMult(deEquis,quinta)(1)
derivadaMult(cubo,cubo)(0.25)
derivadaMult(cte,quinta)(8.90)

derivadaDiv(cubo,cuadrado)(0.76)
derivadaDiv(deEquis,deEquis)(0)
derivadaDiv(cubo,quinta)(-3)
derivadaDiv(cuadrado,cte)(9)
derivadaDiv(cuadrado,quinta)(5)