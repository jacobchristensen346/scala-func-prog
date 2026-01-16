package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0) || (c == r) then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def check(tail: List[Char]): List[Char] =
      if tail.isEmpty then List()
      else if tail.head == '(' && !check(tail.tail).isEmpty then 
        if check(tail.tail).tail.isEmpty then List()
        else 
          check(check(tail.tail).tail)
      else if tail.head == ')' then tail
      else check(tail.tail)
    if chars.isEmpty then true
    else
      if chars.head == '(' then
        if !check(chars.tail).isEmpty then
          balance(check(chars.tail).tail)
        else false
      else if chars.head == ')' then false
      else balance(chars.tail)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if coins.isEmpty then 0
    else if money == 0 then 1
    else if money < 0 then 0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)

