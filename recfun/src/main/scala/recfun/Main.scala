package recfun

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
      if (r < 0 || c < 0) 0
      else if (r == 0 || r == 1) 1
      else if (r == c) pascal(c, r-1)
      else pascal(c, r-1) + pascal(c-1, r -1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanced(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty) count == 0
        else if (chars.head == '(') balanced(chars.tail, count + 1)
        else if (chars.head == ')')
          if (count > 0) balanced(chars.tail, count - 1)
          else false
        else balanced(chars.tail, count)
      }
      balanced(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
