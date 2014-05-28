package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {

  /* "how would you represent the set of all negative integers? You cannot list them all…
     one way would be so say: if you give me an integer, I can tell you whether it’s in the set or not:
     for 3, I say ‘no’; for -1, I say yes."

     NOTA: Sacarse de la cabeza por un momento la idea de Set como una collection de Java
     ====  y pensarlo como una función que recibe un entero y devuelve si pertenece o no al conjunto.
   */
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean   // Otra vez: Set es una función que recibe un Int y devuelve un Boolean!
                              // No importa aún cuál es la función, eso lo decide el que crea el Set.
                              // Acá se articulan los métodos a partir de esa función.

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
  /* "s" es un Set (una función -user defined- que recibe un Int y devuelve Boolean), con lo cual
     s(elem) significa aplicar la función a "elem", por lo que testea la función contra el elemento
     Si da true, el set "contiene" al elemento. */

  /**
   * Returns the set of the one given element.
   */
  /* La idea es devolver una función (que reciba un entero y devuelva booleano)
     que valide para cualquier entrada que sea == al elemento inicial, sino falso */
  def singletonSet(elem: Int): Set = x => elem == x

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */ //Hint: recordar que Set arranca recibiendo un Int para devolver un boolean
  def union(s: Set, t: Set): Set = x => s(x) || t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = x => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */ //Hint: recordar que no devuelve un set concreto de elementos sino una función.
  def diff(s: Set, t: Set): Set = x => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = x => s(x) && p(x)  //Valido que x E s y además aplico el sub filtro p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false //Si no pertenece a `s` paso al siguiente.
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */ //para cada elemento de `s`, alguno debe satisfacer p(x)
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
  //forall(s, x => !p(x)) sería: para toda elemento de `s` si ninguno cumple entonces da True
  //Niego eso (ninguno cumple == false) entonces si alguno cumple == true

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  /*
  Convertir los elementos de S es una función transformada, no parece iteración (no pide usar "forAll").
  "map" transforma un set en otro (una función en otra) aplicando a cada uno de sus elementos la función dada.
  Ej:  Si s(x) = [2]  ^  f(x)=x+1  entonces s(x)=[2], f(x)=[3] y va a devolver el set que hace que solo 3 sea true, no 2.

  def map(s: Set, f: Int => Int): Set = x => if(s(x)) true else false
  pero eso es igual a testear s(x) solo!

  Si pertenece a S => ¿le aplico F? ¿Cómo lo testeo? Porque solo puedo probar por afuera que cumpla F:
  def f = map(s,(x)=>x+1); assert(f(3) === true)

  Los tipos de datos de "f" y de "s" me llevan a hacer
  def map(s: Set, f: Int => Int): Set = x => s(f(x))
  Pero si aplico primero f(2)=3, luego s(3) es false.

  Yo quiero la función inversa, así hago s(f'(x)) y le paso un 3, me convierte en 2 y eso da true.
  Como no tengo una función inversa para toda "f", la única forma que podría hallarla es por fza bruta.

  Por fuerza bruta la inversa sería: "hallar 'x' tal que f(x) sea 'y'" (aplicando f(x) a todos los enteros hasta dar con Y)
  Los "y" serían los elementos de "s":
  def map(s: Set, f: Int => Int): Set = y => `encuentro 'x' tal que` f(x) = y && s(x) == true
  O sea, para cierto Y que me pasen, encuentro X tal que f(x) sea Y, y además s.contains(x).

  El `encuentro 'x' tal que f(x) == y` sería como preguntar "exists" f(x) == y"
  Además, el método exists de acá ya por sí mismo valida sobre el set 's' con lo cual me viene de yapa el && s(x) == true
  */
  def map(s: Set, f: Int => Int): Set = y => exists(s, x => f(x) == y)


  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
