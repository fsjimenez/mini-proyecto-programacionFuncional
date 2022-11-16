
val funcion_1 = (a:Double) => (-(math.pow(a, 2))) + (8*a) - 12 
val funcion_2 = (a:Double) => (3 * (math.pow(a, 2))) 
val funcion_3 = (a:Double) => (a + (2 * (math.pow(a, 2))) - (math.pow(a, 3)) + (5 * (math.pow(a, 4))))
val funcion_4 = (a:Double) => ((2 * a) + 1) / ((math.pow(a, 2)) + a) 
val funcion_5 = (a:Double) => (math.pow(math.E, a)) 
val funcion_6 = (a:Double) => (1/(math.sqrt(a-1)))
val funcion_7 = (a:Double) => ( 1 /  (1 + (a*a))  ) 


//Funcion 1/3 de Simpson Simple

def simple(f:Double=>Double, a:Double, b:Double) : Double = {
  
    def calcularX(a:Double, b:Double) : Double = (a + b)/2
    

    (b - a)   *   ((f(a) + 
                  (4 * (f(calcularX(a, b))))  +  
                  f(b))) / 6

} 

//Funcion 1/3 de Simpson Compuesta

def compuesta(f:Double=>Double, a:Double, b:Double, n:Int) : Double = {
    val h = (b-a)/n

    val calcularX = (a:Double, h:Double, j:Double) => a + j * h

    val rango = (1 to n/2).toList

    (h/3) *  rango.map(x => f(calcularX(a, h, 2*x-2))  +  4 * f(calcularX(a, h, 2*x-1))  +  f(calcularX(a, h, 2*x))).sum

} 

//Funcion 1/3 de Simpson Extendida

def extendida(f:Double=>Double, a:Double, b:Double) : Double = {

    val n = (2 * (b - a)).toInt
    
    val h = (b-a)/n

    val i = (1 to n-1 by 2).toList

    val j = (2 to n-2 by 2).toList

    val sumatoraI = i.map(x => f(a + x * h)).sum

    val sumatoraJ = j.map(x => f(a + x * h)).sum

    (h/3) * (  f(a) +  (4 * sumatoraI) + (2 * sumatoraJ) +  f(b)  )

} 


simple(funcion_1, 3, 5) - 7.33
simple(funcion_2, 0, 2) - 8
simple(funcion_3, -1, 1) - 3.333
simple(funcion_4, 1, 2) - 1.09861
simple(funcion_5, 0, 1) - 1.71828
simple(funcion_6, 2, 3) - 0.828427
simple(funcion_7, 0, 1) - 0.785398

compuesta(funcion_1, 3, 5, 10) - 7.33
compuesta(funcion_2, 0, 2, 10) - 8
compuesta(funcion_3, -1, 1, 10) - 3.333
compuesta(funcion_4, 1, 2, 10) - 1.09861
compuesta(funcion_5, 0, 1, 10) - 1.71828
compuesta(funcion_6, 2, 3, 10) - 0.828427
compuesta(funcion_7, 0, 1, 10) - 0.785398

extendida(funcion_1, 3, 5) - 7.33
extendida(funcion_2, 0, 2) - 8
extendida(funcion_3, -1, 1) - 3.333
extendida(funcion_4, 1, 2) - 1.09861
extendida(funcion_5, 0, 1) - 1.71828
extendida(funcion_6, 2, 3) - 0.828427
extendida(funcion_7, 0, 1) - 0.785398



