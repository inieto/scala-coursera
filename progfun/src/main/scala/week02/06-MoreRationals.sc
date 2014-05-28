class Rational(x: Int, y: Int) {
  //1) Métodos
  def add(that: Rational) = new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
  )
  def neg = new Rational(-numer, denom)
  def sub(that: Rational) = this.add(that.neg)
  //override def toString() = numer + "/" + denom

  //2) Simplify as possible
  private def gcd(a: Int, b:Int): Int =
    if (b == 0) a else gcd(b, a % b)
  //x e y acá son los que vienen en el constructor
  //val numer = x / gcd(x, y)	//numerator
  //val denom = y / gcd(x, y)	//denominator
  //3) Assert y Require
  def less(that: Rational) =
    this.numer * that.denom < that.numer * this.denom
  def max(that: Rational) =
    if(this.less(that)) that else this
  require (y != 0, "denominator must be nonzero")

  //4) Constructores
  def this(x: Int) = this(x,1)	//solo el numerador
  //5) Ejercicio: mantener no simplificados hasta el toString()
  val numer = x
  val denom = y
  override def toString() =
    (numer / gcd(x, y)) + "/" + (denom / gcd(x, y))
}
val x = new Rational(70,49)
val y = 1
y + x.numer   //si hubiera sido internamente simplif me daría 11



