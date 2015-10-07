package main

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

package object Chapter5 {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }
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
            Stream.cons[A](h(), go(i - 1, t()))
          else
            Stream.cons[A](h(), Empty)
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
    def method(n: Int): Int = {
      println(s"this is $n")
      n
    }

    val test = Stream.cons(method(0), Stream.cons(method(1), Stream(method(2))))

    println(test.take(2).toList)
  }
}
