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
          if (i > 1)
            cons[A](h(), go(i - 1, t()))
          else
            cons[A](h(), Empty)
      }

      go(n, this)
    }

    def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), x) if x > 1=> Some((h(), (t(), x - 1)))
      case _ => None
    }

    def drop(n: Int): Stream[A] = {

      @tailrec
      def loop(i: Int, s: Stream[A]): Stream[A] = s match {
        case Empty => Empty
        case Cons(_, t) if i > 1 => loop(i - 1, t())
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

    def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => f(a) append b)

    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }


    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def find(p: A => Boolean): Option[A] = filter(p).headOption

    def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)) {
      case (Cons(h, t), Cons(h1, t1)) => Some((f(h(), h1()), (t(), t1())))
      case _ => None
    }

    def zip[B](s2: Stream[B]): Stream[(A, B)] =
      zipWith(s2)((_, _))

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
      case (Cons(h, t), Cons(h1, t1)) => Some( ( (Some(h()), Some(h1()) ), (t(), t1())) )
      case (Cons(h, t), _) => Some( ( (Some(h()), None), (t(), empty[B]) ) )
      case (_, Cons(h1, t1)) => Some( ( (None, Some(h1())), (empty, t1())))
      case _ => None
    }

    def startsWith[A1 >: A](s: Stream[A1]): Boolean =
      zipAll(s).takeWhile(_._2.isDefined).forAll(p => p._1 == p._2)

    def tails: Stream[Stream[A]] = unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)


    def hasSubsequence[A1 >: A](s: Stream[A1]): Boolean =
      tails.exists( _ startsWith s)


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

    def constant[A](a: A): Stream[A] = {
      // cons(a, constant(a))
      // more efficient book answer:
      lazy val tail: Stream[A] = Cons(() => a, () => tail)
      tail
    }

    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))

    def fromViaUnfold(n: Int): Stream[Int] =
      unfold(n)(n => Some(n, n + 1))

    def constantViaUnfold[A](a: A): Stream[A] =
      unfold(a)(p => Some(p, p))

    def ones: Stream[Int] = unfold(1)(_ => Some(1,1))

    val fibs: Stream[Int] = {
      def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0 + f1))
      go(0, 1)
    }

    val fibsViaUnfold: Stream[Int] = {
      unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case None => empty[A]
      case Some((h, s)) => cons(h, unfold(s)(f))
    }
  }

  def main(args: Array[String]): Unit = {
    println(Stream(2,3,4).hasSubsequence(Stream(3,4)))
  }
}
