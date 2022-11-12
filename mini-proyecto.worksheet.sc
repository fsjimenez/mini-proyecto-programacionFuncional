def integracion(f:Double=>Double, a:Double, b:Double, n:Int) : Double = {
    val h = (b-a)/n

    val calcularX = (a:Double, h:Double, j:Double) => a + j * h

    val rango = (1 to n/2).toList

    (h/3) *  rango.map(x => f(calcularX(a, h, 2*x-2))  +  4 * f(calcularX(a, h, 2*x-1))  +  f(calcularX(a, h, 2*x))).sum

} 

val funcion_1 = (a:Double) => (-(math.pow(a, 2))) + (8*a) - 12 
val funcion_2 = (a:Double) => (3 * (math.pow(a, 2))) 
val funcion_3 = (a:Double) => (a + (2 * (math.pow(a, 2))) - (math.pow(a, 3)) + (5 * (math.pow(a, 4))))
val funcion_4 = (a:Double) => ((2 * a) + 1) / ((math.pow(a, 2)) + a) 
val funcion_5 = (a:Double) => (math.pow(math.E, a)) 
val funcion_6 = (a:Double) => (1/(math.sqrt(a-1)))
val funcion_7 = (a:Double) => ( 1 /  (1 + (a*a))  ) 

integracion(funcion_1, 3, 5, 20)
integracion(funcion_2, 0, 2, 20)
integracion(funcion_3, -1, 1, 20)
integracion(funcion_4, 1, 2, 20)
integracion(funcion_5, 0, 1, 20)
integracion(funcion_6, 2, 3, 20)
integracion(funcion_7, 0, 1, 20)

