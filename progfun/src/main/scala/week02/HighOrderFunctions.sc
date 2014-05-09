def sumInts(a: Int, b: Int): Int = if (a > b) 0 else a + sumInts(a + 1, b)
sumInts(1,4)

def sumId(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, a + acc)
  }
  loop(a, 0)
}
sumId(1, 4)

def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}
sum((x: Int) => x, 1, 4)
sum(x => x * x, 3, 5)