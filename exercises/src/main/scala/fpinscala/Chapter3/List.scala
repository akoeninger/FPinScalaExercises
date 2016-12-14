package main.Chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x, xs) ⇒ x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  // Exercise 3.1 == 3
  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(_, xs) ⇒ xs
  }

  // Exercise 3.3
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil ⇒ Cons(a, Nil)
    case Cons(x, xs) ⇒ Cons(a, xs)
  }

  // Exercise 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _) ⇒ Nil
    case (_, 0) ⇒ l
    case (Cons(_, xs), i) ⇒ drop(xs, i - 1)
  }

  // Exercise 3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A ⇒ Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) ⇒ dropWhile(xs, f)
    case _ ⇒ l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(_, Nil) ⇒ Nil
    case Cons(x, xs) ⇒ Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = as match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) ⇒ x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  /** Exercise 3.7
    *
    * Can product be implemented with short-circuit using foldRight to halt recursion and
    * return 0.0 if it encounters a 0.0?
    *
    * No, foldRight as implemented traverses to the end of the list before the function
    * parameter is even called.
    */

  /** Exercise 3.8
    * Result: Cons(1,Cons(2,Cons(3,Nil)))
    */

  // Exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) ⇒ acc + 1)

  /* Exercise 3.10 */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = as match {
    case Nil ⇒ z
    case Cons(h, t) ⇒ foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) ⇒ acc + 1)

  def reverse[A](as: List[A]) = foldLeft(as, Nil:List[A])((list, value) ⇒ Cons(value, list))

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B =
    foldRight(as, (b: B) ⇒ b)((a, g) ⇒ b ⇒ g(f(b, a)))(z)

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B =
    foldLeft(reverse(as), z)((b, a) ⇒ f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B =
    foldLeft(l, (b: B) ⇒ b)((g, a) ⇒ b ⇒ g(f(a, b)))(z)

  // 3.15 Concatenate a list of lists into a single list. Runtime should be linear to total
  // length of lists.
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def addOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) ⇒ Cons(h+1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) ⇒ Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A ⇒ B): List[B] =
    foldRight(as, Nil: List[B])((h, t) ⇒ Cons(f(h), t))

  def filter[A](as: List[A])(f: A ⇒ Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) ⇒ if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A ⇒ List[B]): List[B] = concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A ⇒ Boolean): List[A] =
    flatMap(as)(a ⇒ if (f(a)) List(a) else Nil: List[A])

  def addLists(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    case Nil ⇒ Nil
    case Cons(h1, t1) ⇒ l2 match {
      case Nil ⇒ Nil
      case Cons(h2, t2) ⇒ Cons(h1 + h2, addLists(t1, t2))
    }
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) ⇒ A): List[A] = l1 match {
    case Nil ⇒ Nil
    case Cons(h1, t1) ⇒ l2 match {
      case Nil ⇒ Nil
      case Cons(h2, t2) ⇒ Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  // Hard
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def checkSub(sup1: List[A], sub1: List[A], matched: List[A]): Boolean = {
      sub1 match {
        case Nil ⇒ true
        case Cons(h1, t1) ⇒ sup1 match {
          case Cons(h2, t2) if h1 == h2 ⇒
            if (checkSub(t2, t1, Cons(h1, matched))) true
            else checkSub(t2, sub, Nil: List[A])
          case Cons(h2, t2) ⇒ hasSubsequence(t2, sub)
        }
      }
    }

    if (length(sup) < length(sub))
      false
    else
      checkSub(sup, sub, Nil: List[A])
  }
}
