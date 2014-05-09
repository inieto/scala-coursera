package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal

  /*
     c0 1  2  3  4  5 6 7
  r   - -  -  -  -  - - -
  0 | 1 0  0  0  0  0 0 0
  1 | 1 1  0  0  0  0 0 0
  2 | 1 2  1  0  0  0 0 0
  3 | 1 3  3  1  0  0 0 0
  4 | 1 4  6  4  1  0 0 0
  5 | 1 5 10 10  5  1 0 0
  6 | 1 6 15 20 15  6 1 0
  7 | 1 7 21 35 35 21 7 1
  r   - -  -  -  -  - - -
     c0 1  2  3  4  5 6 7
  */
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }

  test("pascal; col=4,row=7") {
    assert(pascal(4,7) === 35)
  }

  test("weird case 1") {
    assert(pascal(-1,-1) === 0)
  }

  test("weird case 2") {
    assert(pascal(-1,-5) === 0)
  }

  test("weird case 3") {
    assert(pascal(-5,-1) === 0)
  }

  test("weird case 4") {
    //null.asInstanceOf[Int] evals to 0; Int cannot be Nil
    assert(pascal(null.asInstanceOf[Int],null.asInstanceOf[Int]) === 1)
  }
}
