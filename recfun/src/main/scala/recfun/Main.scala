package recfun
import common._

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
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 && r == 0) 1
    else if (c < 0 || r < 0) 0
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def iter(n: Int, elems: List[Char]): Boolean = {
      if (!elems.isEmpty) 
        if (n < 0) false
        else {
          if (elems.head == '(') iter(n + 1, elems.tail)
          else if (elems.head == ')') iter(n - 1, elems.tail)
          else iter(n, elems.tail)
      } else {
        if (n != 0) false
        else true
      }
    }
    iter(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money == 0) 1
    else if (money >= 0) 
        countChange(money - coins.head, coins) + 
        countChange(money - coins.head, coins.tail)
    else 0
  }
}
