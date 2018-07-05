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

      def pascal(l: List[Int],rn: Int): Int = {
        if (rn == r) l(c)
        else pascal(List(1) ++ l.slice(0,l.size-1).zipWithIndex.map{case(value,index) => l(index)+l(index+1)} ++List(1),rn+1)
      }

      if(r==0) 1
      else pascal(List(1,1),1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balance(l: List[Char],acc: Int): Boolean = {
        if (l.isEmpty) {
          if(acc==0)true else false
        }
        else{
          val result= if (l(0) == '(') 1 else if (l(0) == ')') -1 else 0
          if (acc+result<0) false
          else balance(l.drop(1),acc+result)
        }
      }
      balance(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChange(store: Array[Array[Int]],i: Int,j: Int):Int = {
        if (i==0) 1
        else if(j<0 || i<0) 0
        else if (store(i)(j) >= 0) store(i)(j)
        else {
          store(i)(j) = countChange(store,i-coins(j),j) + countChange(store,i,j-1)
          store(i)(j)
        }
      }
      if (money==0 || coins.size == 0) 0
      else{
        val store = Array.fill(money+1,coins.size)(-1)
        countChange(store,money,coins.size-1)

      }
    }
  }
