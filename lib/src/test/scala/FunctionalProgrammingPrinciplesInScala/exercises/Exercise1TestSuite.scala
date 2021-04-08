package FunctionalProgrammingPrinciplesInScala.exercises

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Exercise1TestSuite extends AnyFunSuite {

  def ex = new Exercise1()

  test("test pascal arguments") {
    assertThrows[java.lang.IllegalArgumentException] {
      ex.pascal(-1, 0)
    }
    assertThrows[java.lang.IllegalArgumentException] {
      ex.pascal(0, -1)
    }
    assertThrows[java.lang.IllegalArgumentException] {
      ex.pascal(1, 0)
    }
  }

  test("test pascal degenerate cases") {
    assertResult(1) {
      ex.pascal(0, 0)
    }
    assertResult(1) {
      ex.pascal(0, 1)
    }
    assertResult(1) {
      ex.pascal(1, 1)
    }
  }

  test("test pascal values") {
    assertResult(1) {
      ex.pascal(0, 4)
    }
    assertResult(4) {
      ex.pascal(1, 4)
    }
    assertResult(6) {
      ex.pascal(2, 4)
    }
    assertResult(4) {
      ex.pascal(3, 4)
    }
    assertResult(1) {
      ex.pascal(4, 4)
    }
  }

  test("test balance values") {
    assertResult(true) {
      ex.balance("".toList)
    }
    assertResult(false) {
      ex.balance("(".toList)
    }
    assertResult(false) {
      ex.balance(")".toList)
    }
    assertResult(false) {
      ex.balance("((".toList)
    }
    assertResult(false) {
      ex.balance("))".toList)
    }
    assertResult(false) {
      ex.balance(")(".toList)
    }
    assertResult(false) {
      ex.balance("(()".toList)
    }
    assertResult(true) {
      ex.balance("()".toList)
    }
    assertResult(true) {
      ex.balance("(())".toList)
    }
    assertResult(true) {
      ex.balance("()()".toList)
    }
  }

  test("test countChange arguments") {
    assertThrows[java.lang.IllegalArgumentException] {
      ex.countChange(-1, List(1))
    }
  }

  test("test countChange degenerate cases") {
    assertResult(0) {
      ex.countChange(0, List.empty[Int])
    }
    assertResult(0) {
      ex.countChange(1, List.empty[Int])
    }
    assertResult(0) {
      ex.countChange(0, List(1))
    }
  }

  test("test countChange values") {
    assertResult(0) {
      ex.countChange(10, List(9))
    }
    assertResult(0) {
      ex.countChange(10, List(11))
    }
    assertResult(1) {
      ex.countChange(10, List(10))
    }
    assertResult(2) {
      ex.countChange(10, List(5,10))
    }
    assertResult(2) {
      ex.countChange(10, List(10,5))
    }
    assertResult(10) {
      ex.countChange(10, List(1,2,5))
    }
  }
}