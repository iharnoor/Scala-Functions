  object Homework6 {
    val x = 1 :: 2 :: 3 :: 4 :: Nil

    def main(args: Array[String]): Unit = {
      println("value of x:" + x)
      println("\nAnswers of 8 Questions are as follows:\n")
      val list = List(1, 1, 1)
      val list1 = List(1, 1, 2, 3)
      println(sumTriple(list))
      println(removeOnes(list))
      println(countOdds(list))
      println(removeAlternating(List(" A", "B", "C", "D", "E")))
      println(isAscending(list1))
      println(addSub(List(10, 20, 30, 40)))
      println(alternate(List(1, 3, 5), List(2, 4, 6)))
      println(fromTo(9, 13))
    }

    /**
      * 1. Write a function called sumTriple that takes a List[Int] and produces an Int. The produced value
      * should be triple the sum of the list of integers.
      *
      * @param xs
      * @return
      */
    def sumTriple(xs: List[Int]): Int = xs match {
      case Nil => 0
      case y :: ys => 3 * y + sumTriple(ys)
    }

    /**
      * 2. Write a function called removeOnes that takes a List[Int] and produces a List[Int]. The produced
      * list should be the same as the input list, but with all ones removed. The order of the elements
      * should not be changed.
      *
      * @param xs
      * @return
      */
    def removeOnes(xs: List[Int]): List[Int] = xs match {
      case Nil => List()
      case y :: ys => if (y == 1) removeOnes(ys) else y :: removeOnes(ys)
    }

    /**
      * 3. Write a function called countOdds that takes a List[Int] and produces an Int that represents the
      * number of odd numbers in the input list.
      *
      * @param xs
      * @return
      */
    def countOdds(xs: List[Int]): Int = xs match {
      case Nil => 0
      case y :: ys => if (y % 2 == 0) countOdds(ys) else 1 + countOdds(ys)
    }

    /**
      * 4. Write a function called removeAlternating that takes a List[String] and produces a List[String] that has every
      * other element in the input list.
      * The first element of the input list must be in the output list. For example:
      * removeAlternating ( List (" A" , "B", "C", "D")) == List (" A","C",)
      *
      * @param xs
      * @return
      */
    def removeAlternating(xs: List[String]): List[String] = xs match {
      case Nil => Nil
      case y :: Nil => List(y)
      case y1 :: y2 :: ys => y1 :: removeAlternating(ys)
    }

    /**
      * 5. Write a function called isAscending that takes a List[Int] and produces a Boolean that is true if the numbers
      * in the input list are in ascending order. Note that the input may have repeated numbers.
      *
      * @param xs
      * @return
      */
    def isAscending(xs: List[Int]): Boolean = xs match {
      case Nil => true
      case y :: ys => ys match {
        case Nil => true
        case z :: zs => if (z >= y) isAscending(ys) else false
      }
    }

    /**
      *6. Write a function called addSub that consumes a List[Int] and produces an Int. The function should add all the elements in even position and subtract all the elements in odd position.
      * Note that the indes of the first element of a list is 0 so it is in even position.
      * For example, addSub(List(10, 20, 30, 40)) should be 10 - 20 + 30 – 40
      *
      * @param xs
      * @return
      */
    def addSub(xs: List[Int]): Int = xs match {
      case x1 :: xs => x1 - addSub(xs)
      case Nil => 0
    }

    /**
      * 7. Write a function called alternate that takes two List[Int] arguments and produces a List[Int]. The elements
      * of the resulting list should alternate between the elements of the arguments. Assume that the two arguments have
      * the same length.
      * For example:
      * alternate ( List (1 , 3, 5) , List (2 , 4, 6)) = List (1 , 2, 3, 4, 5, 6)
      *
      * @param list1 List 1
      * @param list2 List 2
      * @return Integer
      */
    def alternate(list1: List[Int], list2: List[Int]): List[Int] = list1 match {
      case x :: xs => x :: alternate(list2, xs)
      case Nil => list2
    }

    /**
      * 8. Write a function called fromTo that takes two Ints as arguments and produces a List[Int]. The value of
      * fromTo(x, y) should be the list of consecutive integers that start from and include x, going up to and excluding y.
      * Assume that x < y.
      * For example:
      * fromTo (9 , 13) = List (9 , 10 , 11 , 12)
      *
      * @param a
      * @param b
      * @return
      */
    def fromTo(a: Int, b: Int): List[Int] = if (a < b) a :: fromTo(a + 1, b) else List()
  }

