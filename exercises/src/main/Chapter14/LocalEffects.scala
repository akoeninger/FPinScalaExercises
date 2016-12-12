package main.Chapter14

import language.implicitConversions
import scala.collection.mutable

import main.Chapter11._

object Mutable {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
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

  def fill(xs: Map[Int,A]): ST[S,Unit] = xs.foldRight(Immutable.noop[S]) {
    case ((k, v), st) => st.flatMap(_ => write(k, v))
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S,A]] = ST(new STArray[S,A] {
    override protected lazy val value: Array[A] = Array.fill(sz)(v)
  })

  def fromList[S,A: Manifest](xs: List[A]): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      override protected lazy val value: Array[A] = xs.toArray
    })
}

sealed abstract class STHashMap[S, K, V] {

  protected def table: mutable.HashMap[K, V]

  def size: ST[S, Int] = ST(table.size)

  def put(k: K, v: V): ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      table.put(k, v)
      ((), s)
    }
  }

  def +=(kv: (K, V)): ST[S, Unit] = ST(table += kv)

  def -=(key: K): ST[S, Unit] = ST(table -= key)

  def get(key: K): ST[S, Option[V]] = ST(table.get(key))

  def apply(k: K): ST[S, V] = ST(table(k))

}

object STHashMap {
  import scala.collection.mutable.HashMap
  def empty[S, K, V]: ST[S, STHashMap[S, K, V]] = ST(new STHashMap[S,K,V] {
    override protected def table: mutable.HashMap[K, V] = mutable.HashMap.empty[K, V]
  })

  def fromMap[S,K,V](m: Map[K,V]): ST[S, STHashMap[S,K,V]] = ST(new STHashMap[S,K,V] {
    override protected def table: mutable.HashMap[K, V] = (mutable.HashMap.newBuilder ++= m).result()
  })
}

object Immutable {
  def noop[S]: ST[S, Unit] = ST[S, Unit](())

  def partition[S](arr: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
    pivotVal <- arr.read(pivot)
    _ <- arr.swap(pivot, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop[S])((s, i) =>
      for {
        _ <- s
        vi <- arr.read(i)
        _ <- if (vi < pivotVal)
          for {
            vj <- j.read
            _ <- arr.swap(i, vj)
            _ <- j.write(vj + l)
          } yield ()
        else noop[S]
      } yield ())
    x <- j.read
    _ <- arr.swap(x, r)
  } yield x

  def qs[S](a: STArray[S,Int], n: Int, r: Int): ST[S, Unit] =
    if (n < r) {
      for {
        pi <- partition(a, n, r, n + (r - n) / 2)
        _ <- qs(a, n, pi - 1)
        _ <- qs(a, pi + 1, r)
      } yield ()
    } else {
      noop[S]
    }

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      override def apply[S]: ST[S, List[Int]] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })
}

