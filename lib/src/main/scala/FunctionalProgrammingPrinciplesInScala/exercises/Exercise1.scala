package FunctionalProgrammingPrinciplesInScala.exercises

class Exercise1 {

  /*
   * Compute the value of Pascal's triangle at position (c, r)
   */
  def pascal(c: Int, r: Int): Int = {
    require(c >= 0 && r >= 0, "arguments must be greater than 0")
    require(r >= c, "argument r must be greater or equal to argument c")

    if (c == 0 || c == r)
      1
    else
      pascal(c, r-1) + pascal(c-1, r-1)
  }

  /*
   * Check if a list of chars has balanced parentheses
   */
  def balance(chars: List[Char]): Boolean = {
    val ps = chars.filter(c => c == ')' || c == '(')

    def reduce(acc: List[Char], leftover: List[Char], flag: Boolean): List[Char] = {
      if (leftover.isEmpty)
        acc
      else if (leftover.size == 1 && flag == false)
        acc ++ leftover
      else if (leftover.size == 1 && flag == true)
        reduce(List.empty[Char], acc ++ leftover, false)
      else if (leftover.size > 1 && leftover.head == '(' && leftover.tail.head == ')')
        reduce(acc, leftover.tail.tail, true)
      else if (leftover.size > 1 && leftover.head == '(' && leftover.tail.head == '(')
        reduce(acc :+ '(', leftover.tail, false)
      else
        reduce(leftover, List.empty[Char], false)
    }

    if (reduce(List.empty[Char], ps, false).isEmpty) {
      true
    }
    else {
      false
    }
  }

  /*
   * Collect possible combinations of coins. The strategy we use is that all combinations of coins
   * c1, ... c_n can be expressed in terms of those combinations that use c1 and those that
   * do not. This allows us to use recursion to decrease the space of coins to guarantee
   * that the algorithm terminates.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    require(money >= 0, "money must be greater or equal to 0")

    // Assume coins are unique
    def makeChange(money: Int, coins: List[Int], acc: List[Int]): List[List[Int]] = {
      val eligibleCoins = coins.filter(c => c <= money)

      if (money == 0) {
        acc :: Nil
      }
      else if (eligibleCoins.isEmpty) {
        Nil
      }
      else {
        val coinToUse = eligibleCoins.head
        val leftovers = eligibleCoins.tail
        makeChange(money - coinToUse, eligibleCoins, acc :+ coinToUse) ++ makeChange(money, leftovers, acc)
      }
    }

    val coinsList = coins.distinct
    makeChange(money, coinsList, List.empty[Int]).filter(!_.isEmpty).distinct.size
  }

}

