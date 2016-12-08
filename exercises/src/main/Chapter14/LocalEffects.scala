package main.Chapter14

import language.implicitConversions
import main.Chapter11._

object Mutable {
  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty)
      xs
    else {
      val arr = xs.toArray
      def swap(x: Int, y: Int) = {
        val tmp = arr(x)
        arr(x) = arr(y)
        arr(y) = tmp
      }
      def partition(l: Int, r: Int, pivot: Int) = {
        val pivotVal = arr(pivot)
        swap(pivot, r)
        var j = l
        for (i <- l until r) if (arr(i) < pivotVal) {
          swap(i, j)
          j += l
        }
        swap(j, r)
        j
      }
      def qs(l: Int, r: Int): Unit = if (l < r) {
        val pi = partition(l, r, l + (r - l) / 2)
        qs(l, pi - 1)
        qs(pi + 1, r)
      }
      qs(0, arr.length - 1)
      arr.toList
    }
}

sealed trait ST[S,A] { self =>
  protected def run(s: S): (A, S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      override protected def run(s: S): (A, S) = (memo, s)
    }
  }
  def runST[A](st: RunnableST[A]): A = st.apply[Unit].run(())._1
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    override protected var cell: A = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S,A]
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))
  def freeze: ST[S, List[A]] = ST(value.toList)
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S,A]] = ST(new STArray[S,A] {
    override protected lazy val value: Array[A] = Array.fill(sz)(v)
  })
}
