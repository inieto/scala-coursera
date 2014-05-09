package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
       c0 1 2 3 4
    r   - - - - -
    0 | 1 0 0 0 0     c > r == 0 or throw new NoSuchElementException?
    1 | 1 1 0 0 0     p(c0,r1) = 1; p(c1,r1) = p(c0,r0) + p(c1,r0) --> then need pascal(c1,r0) to be 0
    2 | 1 2 1 0 0     p(c1,r2) = p(c0,r1) + p(c1,r1), etc
    3 | 1 3 3 1 0
    4 | 1 4 6 4 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c > r || c < 0 || r < 0) 0      //off limits gives 0
    else if (c == 0 || c == r) 1        //Column 0 and Diagonal are "1"
    else if (c == 1 || c == (r - 1)) r  //avoiding recursion for simple corners
      else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /* What about tailrec? Not that simple for pascal. See
  http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html
  http://blog.higher-order.com/assets/trampolines.pdf
*/

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def iter(deep: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) deep == 0
      else if (chars.head == '(')  iter(deep + 1, chars.tail)
      else if (chars.head == ')')  deep > 0 && iter(deep - 1, chars.tail)
      else iter(deep, chars.tail)
    }
    iter(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def iter(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else iter(money, coins.tail) + iter(money - coins.head, coins)
    }
    if (money == 0) 0
    else iter(money, coins)
    //Sorting Lists: coins.sortWith(_ > _) or coins.sortWith((x,y) => x > y)
  }
}
