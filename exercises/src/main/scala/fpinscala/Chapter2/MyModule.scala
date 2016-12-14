package fpinscala.Chapter2

import scala.annotation.tailrec

object MyModule {

  // Exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def loop(i: Int, p1: Int, p2: Int): Int =
      if (i <= 0) p1 else loop(i - 1, p2, p1 + p2)

    loop(n, 0, 1)
  }

  // Exercise 2.4
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (ordered(as(n-1), as(n))) loop(n + 1)
      else false
    }

    if (as.nonEmpty) loop(1) // starting at 1 will return true for single element arrays
    else true
  }


  /* Exercise 2.3 */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
