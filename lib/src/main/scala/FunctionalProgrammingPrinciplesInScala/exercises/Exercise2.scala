package FunctionalProgrammingPrinciplesInScala.exercises

class Exercise2 {

  /*
   * Given function definitions
   */
  type FunSet = Int => Boolean
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): FunSet = (x: Int) => x == elem
  def union(s: FunSet, t: FunSet): FunSet = x => s(x) || t(x)
  def intersect(s: FunSet, t: FunSet): FunSet = x => s(x) && t(x)
  def diff(s: FunSet, t: FunSet): FunSet = x => s(x) && !t(x)
  def filter(s: FunSet, p: Int => Boolean): FunSet = x => s(x) && p(x)

  /*
   * Only check values of integers between -1000 and 1000
   * Function should return true if p(x) is true for all x in S
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) true
      else if (s(a) && !p(a)) false
      else iter(a+1)
    }
    iter(-1000)
  }

  /*
   * Function should return true if p(x) is true for some x in S
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  /*
   * Apply f to each element in s to obtain a new function set
   */
  def map(s: FunSet, f: Int => Int): FunSet = x => exists(s, y => f(y) == x)

}

