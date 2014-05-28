def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  val tolerance = 0.001

  def isCloseEnough(x: Double, y: Double)=
    Math.abs((x - y) / x) / x < tolerance

  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}
fixedPoint(x => 1+x/2)(1)
def sqrt(x: Double) = fixedPoint(y => (y + x/y) / 2)(x)
sqrt(2)

def averageDamp(f: Double => Double)(x: Double)=(x + f(x))/2

// Escribir una "sqrt" que use "averageDamp" y "fixedPoint"
def sqrt2(x: Double) =
  fixedPoint(averageDamp(y => x / y))(x)  //firstGuess puede ser 1 o x, total converge
sqrt2(2)
