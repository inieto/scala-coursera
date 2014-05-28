class Rational(x: Int, y: Int) {
  def numer = x	//numerator
  def denom = y	//denominator

  def add(that: Rational) =
    new Rational(
      numer * that.denom +
      that.numer * denom,
      denom * that.denom
    )

  def neg = new Rational(-numer, denom)
  //anda también (-x,y), mi primer intento

  def sub(that: Rational) =
    this.add(that.neg)  //Odersky lo hizo sin el this.

  override def toString() =
    numer + "/" + denom
}
val x = new Rational(1,3)
x.numer
x.denom
val y = new Rational(5,7)
x.add(y)

/* Ejercicio: x = 1/3, y = 5/7, z = 3/2
//x.add(y).mul(z)
1) Implementar x.neg //devuelve -x
2) Implementar x.sub(Rational)	//hace la resta
3) Calcular el resultado de x-y-z   */
val z = new Rational(3,2)
x.sub(y).sub(z)

