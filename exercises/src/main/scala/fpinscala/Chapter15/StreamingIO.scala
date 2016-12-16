package fpinscala.Chapter15

import language.{higherKinds, implicitConversions, postfixOps}

import fpinscala.Chapter13.{Free, IO, Monad, unsafePerformIO}

object ImperativeAndLazyIO {
  def linesGt40k(filename: String): IO[Boolean] = IO {
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines()
      while (count <= 40000 && lines.hasNext) {
        lines.next()
        count += 1
      }
      count > 40000
    }
    finally src.close()
  }

  /*
The above code is rather low-level, and it's not compositional,
either. Consider the following scenarios:
* Check whether the number of _nonempty_ lines in the file exceeds
40,000
* Find a line index before 40,000 where the first letter of
consecutive lines spells out `"abracadabra"`.
We cannot just compose our existing implementation with some
other combinator(s) to implement these tasks. Our implementation is
a monolithic loop, and we must modify this loop directly if we want
to change its behavior.
Now imagine if we had a `Stream[String]` for the lines of the file
and we could assemble functionality using all the `Stream` functions
we know and love.
   */

  object Examples {
    val lines: Stream[String] = sys.error("defined elsewhere")
    val ex1 = lines.zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex2 = lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex3 = lines.take(40000).map(_.head).indexOfSlice("abracadabra".toList)
  }

  /* Cheat to return IO[Stream[String]] to represent the lines of the file. "Lazy I/O" */
  def lines(filename: String): IO[Stream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines().toStream append { src.close(); Stream.empty }
  }
}

object SimpleStreamTransducers {
  /*
    We now introduce a type, `Process`, representing pure, single-input
    stream transducers. It can be in of three states - it can be
    emitting a value to the output (`Emit`), reading a value from its
    input (`Await`) or signaling termination via `Halt`.
                               */
  sealed trait Process[-I, +O] {
    import Process._
    def apply(s: Stream[I]): Stream[O] = this match {
      case Emit(head, tail) => head #:: tail(s)
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Halt() => Stream()
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Emit(head, tail) => Emit(head, go(tail))
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Halt() => go(this)
      }
      go(this)
    }
  }

  object Process {
    case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
    case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
    case class Halt[I, O]() extends Process[I, O]

    def emit[I, O](h: O, t: Process[I, O] = Halt[I, O]()): Process[I, O] = Emit(h, t)

    def await[I, O](
      f: I => Process[I, O],
      fallback: Process[I, O] = Halt[I, O]()
    ): Process[I ,O] = Await {
      case Some(i) => f(i)
      case None => fallback
    }

    def liftOne[I, O](f: I => O): Process[I, O] = Await {
      case Some(i) => emit(f(i))
      case None => Halt()
    }

    def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

    def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
      case Some(i) if p(i) => emit[I, I](i)
      case _ => Halt()
    }.repeat

    def sum: Process[Double, Double] = loop(0.0)((d, s) => (d + s, d + s))

    def take[I](n: Int): Process[I, I] = {
      if (n <= 0) Halt()
      else await[I, I](i => emit(i, take[I](n - 1)))
    }

    def drop[I](n: Int): Process[I, I] = {
      if (n <= 0) id
      else await[I, I](i => drop[I](n - 1))
    }

    def takeWhile[I](f: I => Boolean): Process[I, I] = await(i =>
      if (f(i)) emit(i, takeWhile(f))
      else Halt()
    )

    def dropWhile[I](f: I => Boolean): Process[I, I] = await(i =>
      if (f(i)) dropWhile(f)
      else emit(i, id)
    )

    def id[I]: Process[I,I] = lift(identity)

    def count[I]: Process[I, Int] = loop(0)((_, s) => (s + 1, s + 1))

    def mean: Process[Double, Double] = {
      def go(accMean: Double, count: Int): Process[Double, Double] = Await {
        case Some(x) =>
          val count1 = count + 1
          val mean = accMean + (x - accMean) / count1
          emit(mean, go(mean, count1))
        case None => Halt()
      }
      go(0.0, 0)
    }

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = await((i: I) =>
      f(i,z) match {
        case (o, s2) => emit(o, loop(s2)(f))
      }
    )
  }
}


