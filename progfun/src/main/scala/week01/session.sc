1+3

def abs(x: Double) = if (x < 0) -x else x

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double) =
  //abs(guess * guess - x) < 0.001  // Falla (*)
  abs(guess * guess /x - 1) < 0.001 //Mejorada

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def sqrt (x: Double) = sqrtIter(1.0, x)
sqrt(2)
sqrt(4)
sqrt(1e-6)  //0.001 (*) Da 0.03126 debería ser 1e-3
sqrt(1e60)  //1     (*) Da StackOverflowError

def sqrt2(x: Double) = {
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  def improve(guess: Double, x: Double) =
    (x/guess + guess) / 2
  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) / x < 0.001
  sqrtIter(1.0, x)
}
sqrt2(4)

def factorialTailRec(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc
    else loop(acc * n, n-1)
  loop(1, n)
}

//factorialTailRec(4) = loop(1,4) =
//if(4 == 0) 1 else loop(1*4,3)=...=
//if(3 == 0) 4 else loop(4*3,2)=...=
//if(2 == 0) 12 else loop(12*2,1)=...=
//if(1 == 0) 24 else loop(24*1,0)=...=
//if(0 == 0) 24 else loop(24*0,-1)= 24
factorialTailRec(4)