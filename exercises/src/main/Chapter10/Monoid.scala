package main.Chapter10

import language.higherKinds

import main.Chapter7.Nonblocking._
import main.Chapter7.Nonblocking.Par._
import main.Chapter8._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override val zero = 0

    override def op(a1: Int, a2: Int) = a1 + a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override val zero = 1

    override def op(a1: Int, a2: Int) = a1 * a2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override val zero = false

    override def op(a1: Boolean, a2: Boolean) = a1 || a2
  }


  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override val zero = true

    override def op(a1: Boolean, a2: Boolean) = a1 && a2
  }

  def dual[A](monoid: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = monoid.op(a2, a1)

    override def zero = monoid.zero
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero = None

    override def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
  }

  private def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  private def lastOptionMonoid[A]: Monoid[Option[A]] = dual(optionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2

    override def zero = identity
  }

  def composeEndoMonoid[A]: Monoid[A => A] = dual(endoMonoid)

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(gen) { a =>
    (m.op(a, m.zero) == m.op(m.zero, a)) && (m.op(m.op(a, a), m.zero) == m.op(a, m.op(a, m.zero)))
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, b) => m.op(acc, f(b)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val l = as.length
    if (l == 0)
      m.zero
    else if (l == 1)
      f(as(0))
    else {
      val (left, right) = as.splitAt(l / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val orderMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]) = (a1, a2) match {
        case (Some((x1, y1, p1)), Some((x2, y2, p2))) =>
          Some(x1 min x2, y1 max y2, p1 && p2 && y1 <= x2)
        case (x, None) => x
        case (None, x) => x
      }

      override def zero = None
    }
    foldMapV(ints, orderMonoid)(i => Some(i, i, true)).forall(_._3)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]) = a1.map2(a2)(m.op)

    override def zero = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap(bs => foldMapV(bs, par(m))(Par.lazyUnit))

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC) = (a1, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l, wc, r)) => Part(a + l, wc, r)
      case (Part(l, wc, r), Stub(a)) => Part(l, wc, r + a)
      case (Part(l1, wc1, r1), Part(l2, wc2, r2)) =>
        val hasNewWord = (r1 + l2).nonEmpty
        Part(l1, wc1 + (if (hasNewWord) 1 else 0) + wc2, r2)
    }

    override def zero = Stub("")
  }

  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(s: String): Int = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(chars) => unstub(chars)
      case Part(lStub, words, rStub) => unstub(lStub) + words + unstub(rStub)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(value) => f(value)
    case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(value) => f(z, value)
    case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(value) => f(value, z)
    case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case Some(x) => f(z, x)
    case None => z
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
}

