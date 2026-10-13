import main.Derivacion.*

val deEquis = (x :Double) => x
val cuadrado = (x :Double) => x*x
val cubo = (x :Double) => x*x*x
val cte = (x :Double) => x*x*x*x
val quinta = (x :Double) => x*x*x*x*x

derivadaSuma(cte,cuadrado)(4)
derivadaSuma(cubo,cuadrado)(10)
derivadaSuma(cubo,quinta)(8)
derivadaSuma(quinta,cte)(10)
derivadaSuma(deEquis,cuadrado)(80)