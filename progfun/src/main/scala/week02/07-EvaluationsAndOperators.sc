class Rational(x: Int, y: Int) {
  require (y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x,1)	//solo el numerador

  private def gcd(a: Int, b:Int): Int = if (b == 0) a else gcd(b, a % b)
  val numer = x / gcd(x, y)	//numerator
  val denom = y / gcd(x, y)	//denominator

  //def neg = new Rational(-numer, denom)
  def unary_- : Rational = new Rational(-numer,denom) //único caso especial de convención en Scala

  def + (that: Rational) = new Rational(
    numer * that.denom + that.numer * denom,
    denom * that.denom
  )

  def - (that: Rational) = this + -that //that.neg

  def < (that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this < that) that else this

  override def toString() = numer + "/" + denom
}

val x = new Rational(1)
val y = new Rational(1,2)

val z = x + y