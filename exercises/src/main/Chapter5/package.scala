package main

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

package object Chapter5 {

  sealed trait Stream[+A] {
    import Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def headOptionViaFoldRight: Option[A] =
      foldRight(None: Option[A])((a, acc) => Some(a))

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def toListFast: List[A] = {
      val buf = new ListBuffer[A]

      @tailrec
      def go(s: Stream[A]): List[A] = s match {
        case Empty => buf.toList
        case Cons(h, t) =>
          buf += h()
          go(t())
      }
      go(this)
    }

    def take(n: Int): Stream[A] = {
      def go(i: Int, s: Stream[A]): Stream[A] = s match {
        case Empty => Empty
        case Cons(h, t) =>
          if (i > 0)
            cons[A](h(), go(i - 1, t()))
          else
            cons[A](h(), Empty)
      }

      go(n, this)
    }

    def drop(n: Int): Stream[A] = {

      @tailrec
      def loop(i: Int, s: Stream[A]): Stream[A] = s match {
        case Empty => Empty
        case Cons(_, t) if i > 0 => loop(i - 1, t())
        case Cons(_, t) => t()
      }
      loop(n, this)
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      def go(s: Stream[A]): Stream[A] = s match {
        case Empty => Empty
        case Cons(h, t) =>
          lazy val head = h()

          if (p(head))
            cons(head, go(t()))
          else
            empty

      }
      go(this)
    }

    def map[B](f: A => B): Stream[B] =
      foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => f(a) append b)

    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    println(Stream(1,2,3,4,5,6,7,8,9,10).append(Stream(11)).toList)
  }
}
