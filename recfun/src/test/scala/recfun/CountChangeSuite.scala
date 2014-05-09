package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  } // 2+2, 1+1+2, 1+1+1+1

  test("countChange: similar to example given in instructions") {
    assert(countChange(5,List(1,2)) === 3)
  } // 2+2+1, 1+1+2+1, 1+1+1+1+1

  test("countChange: countChange(6,List(1,2) === 4") {
    assert(countChange(6,List(1,2)) === 4)
  } // 2+2+2, 1+1+2+2, 1+1+1+1+2, 1+1+1+1+1+1

  test("countChange: countChange(6,List(1,2,3) === 7") {
    assert(countChange(6,List(1,2,3)) === 7)
  } // 3+3, 2+2+2, 2+1+3, 1+1+2+2, 1+1+1+1+2, 1+1+1+1+1+1, 3+1+1+1

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

  test("countChange: no money") {
    assert(countChange(0,List(1,2)) === 0)
  }

  test("countChange: no coins") {
    assert(countChange(4,Nil) === 0)
  }
}
