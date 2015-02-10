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
   * @param c column number
   * @param r row number
   * @return int - value in pascal's triangle at (c,r)
   */
  def pascal(c: Int, r: Int): Int = if (c == 0 || r == 0 || c == r) 1 else pascal(c-1,r-1) + pascal(c,r-1)
  /**
   * Exercise 2
   * @param chars char list input to be checked
   * @return bool - if the input had balanced parens
   */
  def balance(chars: List[Char]): Boolean = {

    def aux(chars: List[Char], numOpen: Int): Boolean = {

      if (numOpen < 0) false
      else
        chars match {
          case Nil       => numOpen == 0
          case '(' :: xs => aux(xs,numOpen+1)
          case ')' :: xs => aux(xs,numOpen-1)
          case _ :: xs   => aux(xs,numOpen)
        }
    }
    aux(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    //uniquify inputs and

    (money,coins) match {

      case (0,  _) => 1 //one way to make change for $0
      case (_,Nil) => 0 //no way to make change without coins
      case (m, c::cs)  => if (m - c == 0) 1 + countChange(m, cs) //if we make perfect change: +1 and try last bit with dif coins
                          else if (c > m) countChange(m, cs) //if coin won't fit, try remaining coins
                               else countChange(m, cs) + countChange(m-c, c::cs) //not using + using it
    }
  }
}
