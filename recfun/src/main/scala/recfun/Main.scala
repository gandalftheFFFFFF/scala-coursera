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
      if (c == r || c == 0 || r == 0)
        1
      else
        pascal(c, r - 1) + pascal(c - 1, r - 1)      
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def help(xs: List[Char], level: Int): Boolean = {
        if (xs.isEmpty) level == 0
        else if (xs.head == ')' && level == 0) false
        else if (xs.head == '(') help(xs.tail, level + 1)
        else if (xs.head == ')' && level > 0) help(xs.tail, level - 1)
        else help(xs.tail, level)
      }
      help(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def help(money: Int, coins: List[Int], sum: Int): Int = {
        if (money < 0) sum
        else
          if (coins.isEmpty)
            if (money == 0) sum + 1 else sum
          else
            help(money - coins.head, coins, sum) + help(money, coins.tail, sum)
      }
      help(money, coins, 0)
    }
  }
