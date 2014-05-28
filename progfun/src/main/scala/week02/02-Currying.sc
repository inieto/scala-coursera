//Ejercicio 1: Definir una función product que multiplique los resultados devueltos por una función

def product(f: Int => Int)(a: Int, b: Int) : Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
//Es medio al pedo pasar la función como parámetro como placeholder para modificar los datos
//si solo vamos a pasar la función identidad

product(x => x*x) (1,5)   //5! = 120


//Ejercicio 2: Definir el factorial en función del producto
//Factorial: 1 * 2 * ... * n, entonces tampoco necesito el placeholder f
def fact(n: Int) =  product (x => x)(1,n)

fact(0)   //0! = 1
fact(5)   //5! = 120


//Ejercicio 3: Generalizar el sum y el product.
def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)
// Lo que varía es la condición de corte y el operador +/*

// Ni en pedo se me iba a ocurrir esta firma y llamarlo "mapReduce"
def mapReduce(f: Int => Int,
    combine: (Int, Int) => Int, zero: Int) (a: Int, b: Int): Int =
  if (a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

// Otra vez la funcion al pedo "f" como identidad, el 1 como caso borde
// está bueno pasarle la multiplicación como x,y => x*y
mapReduce (x=>x, (x,y) => x*y, 1) (1,5)

// Defino ahora prod y sum como casos especiales de MapReduce
def prod(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x,y) => x*y, 1)(a,b)
def suma(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x,y) => x+y, 0)(a,b)
prod(x=>x)(1,5)	//5! = 120
suma(x=>x)(1,5)	//15

//Y uno para hacer valer la pena la "f" al final:
def sumCubes(a: Int, b: Int) = mapReduce(x => x*x*x, (x,y) => x+y, 0)(a,b)
sumCubes(1,3)   //1^3 * 2^3 * 3^3 = 1 * 8 * 27 = 216