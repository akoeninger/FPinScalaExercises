package fpinscala.Chapter15

import java.io.{File, FileWriter}

import language.{higherKinds, implicitConversions, postfixOps}
import scala.util.{Left, Right}

import fpinscala.Chapter13.{Free, IO, Monad, Monadic, unsafePerformIO}

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
  sealed trait Process[I, O] {
    import SimpleStreamTransducers.Process._
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

    def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
      case Halt() => Halt()
      case Emit(head, tail) => Emit(head, this |> tail)
      case Await(f) => this match {
        case Halt() => Halt() |> f(None)
        case Emit(head, tail) => tail |> f(Some(head))
        case Await(g) => Await((i: Option[I]) => g(i) |> p2)
      }
    }

    def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Emit(head, tail) => Emit(head, tail ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
      case Halt() => p
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(head, tail) => f(head) ++ tail.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    }

    def zip[O2](p: Process[I, O2]): Process[I, (O, O2)] = Process.zip(this, p)

    //      this.map2[Int, (O, Int)](count[I])((o, i) => (o, i - 1))
    def zipWithIndex: Process[I, (O, Int)] = this.zip(count map (_ - 1))
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

    def monad[I]: Monad[({ type f[x] = Process[I, x]})#f] = new Monad[({type f[x] = Process[I, x]})#f] {
      override def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = p flatMap f

      override def unit[O](o: => O): Process[I, O] = Emit(o)
    }

    implicit def toMonadic[I, O](a: Process[I, O]): Monadic[({type f[x] = Process[I, x]})#f, O] =
      monad[I].toMonadic(a)

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

    def mean2: Process[Double, Double] = zip(sum, count[Double]).map(t => t._1 / t._2)

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = await((i: I) =>
      f(i,z) match {
        case (o, s2) => emit(o, loop(s2)(f))
      }
    )

    def zip[I, O, O2](p1: Process[I, O], p2: Process[I, O2]): Process[I, (O, O2)] = (p1, p2) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
      case (Await(recv1), _) => Await((oi: Option[I]) => zip(recv1(oi), feed(oi)(p2)))
      case (_, Await(recv2)) => Await((oi: Option[I]) => zip(feed(oi)(p1), recv2(oi)))
    }

    def feed[I, O](oi: Option[I])(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => p
      case Emit(head, tail) => Emit(head, feed(oi)(tail))
      case Await(recv) => recv(oi)
    }

    def exists[I](f: I => Boolean): Process[I, Boolean] = lift(f) |> any

    def any: Process[Boolean, Boolean] = loop(false)((b: Boolean, s) => (s || b, s || b))

    def processFile[A, B](
      f: File,
      p: Process[String, A],
      z: B
    )(g: (B, A) => B): IO[B] = IO {
      @annotation.tailrec
      def go(ss: Iterator[String], cur: Process[String, A], acc: B): B = cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))
      }
      val s = io.Source.fromFile(f)
      try go(s.getLines(), p, z)
      finally s.close()
    }

    def convertFahrenheit: Process[String, String] = {
      def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

      filter[String](ln => ln.nonEmpty && !ln.startsWith("#")) |>
        lift(st => toCelsius(st.toDouble).toString)
    }
  }
}

object GeneralizedStreamTransducers {

  /*
Our generalized process type is parameterized on the protocol used for
communicating with the driver. This works similarly to the `IO` type
we defined in chapter 13. The `Await` constructor emits a request of
type `F[A]`, and receives a response of type `Either[Throwable,A]`:
trait Process[F,A]
case class Await[F[_],A,O](
req: F[A],
recv: Either[Throwable,A] => Process[F,O]) extends Process[F,O]
case class Halt[F[_],O](err: Throwable) extends Process[F,O]
case class Emit[F[_],O](head: O, tail: Process[F,O]) extends Process[F,O]
The `Await` constructor may now receive a successful result or an error.
The `Halt` constructor now has a _reason_ for termination, which may be
either normal termination indicated by the special exception `End`,
forceful terimation, indicated by the special exception `Kill`,
or some other error.
We'll use the improved `Await` and `Halt` cases together to ensure
that all resources get released, even in the event of exceptions.
*/

  trait Process[F[_], O] {
    import fpinscala.Chapter15.GeneralizedStreamTransducers.Process._

    def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
      case Halt(err) => Try(f(err))
      case Emit(head, tail) => Emit(head, tail.onHalt(f))
      case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
    }

    def ++(p: => Process[F, O]): Process[F, O] = this.onHalt {
      case End => p
      case err => Halt(err)
    }

    def onComplete(p: => Process[F, O]): Process[F, O] = this.onHalt {
      case End => p.asFinalizer
      case err => p.asFinalizer ++ Halt(err)
    }

    private def asFinalizer: Process[F, O] = this match {
      case Emit(h, t) => Emit(h, t.asFinalizer)
      case Halt(e) => Halt(e)
      case Await(req, recv) => await(req) {
        case Left(Kill) => this.asFinalizer
        case x => recv(x)
      }
    }

    def repeat: Process[F, O] = this ++ this.repeat


    def |>[O2](p2: Process1[O, O2]): Process[F, O2] = p2 match {
      case Halt(err) => this.kill onHalt { Halt(err) ++ Halt(e2)}
      case Emit(h, t) => Emit(h, this |> t)
      case Await(req0, recv0) => await(req0)(recv0 andThen (_ |> p2))
    }

    def pipe[O2](p2: Process1[O, O2]): Process[F, O2] = this |> p2

    @annotation.tailrec
    final def kill[O2]: Process[F, O2] = this match {
      case Await(req, recv) => recv(Left(Kill)).drain.onHalt {
        case Kill => Halt(End)
        case e => Halt(e)
      }
      case Halt(e) => Halt(e)
      case Emit(h, t) => t.kill
    }

    def filter(f: O => Boolean): Process[F, O] = this |> Process.filter(f)

    final def drain[O2]: Process[F, O2] = this match {
      case Halt(err) => Halt(err)
      case Emit(head, tail) => tail.drain
      case Await(req, recv) => Await(req, recv andThen(_.drain))
    }

    def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
      case Halt(err) => Halt(err)
      case Emit(head, tail) => Try(f(head)) ++ tail.flatMap(f)
      case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
    }

    def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
      def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = cur match {
        case Emit(head, tail) => go(tail, acc :+ head)
        case Halt(End) => F.unit(acc)
        case Halt(err) => F.fail(err)
        case Await(req, recv) => F.flatMap(F.attempt(req)) { e => go(Try(recv(e)), acc) }
      }
      go(this, IndexedSeq())
    }

    def tee[O2, O3](p2: Process[F, O2])(t: Tee[O, O2, O3]): Process[F, O3] = t match {
      case Halt(e) => this.kill onComplete p2.kill onComplete Halt(e)
      case Emit(h, tail) => Emit(h, (this tee p2)(t))
      case Await(side, recv) => side.get match {
        case Left(isO) => this match {
          case Halt(e) => p2.kill onComplete Halt(e)
          case Emit(o, ot) => (ot tee p2)(Try(recv(Right(o))))
          case Await(reqL, recvL) => await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
        }
        case Right(isO2) => p2 match {
          case Halt(e) => this.kill onComplete Halt(e)
          case Emit(o2, ot) => (this tee ot)(Try(recv(Right(o2))))
          case Await(reqR, recvR) => await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
        }
      }
    }

    def zipWith[O2,O3](p2: Process[F,O2])(f: (O,O2) => O3): Process[F,O3] =
      (this tee p2)(Process.zipWith(f))

    def zip[O2](p2: Process[F,O2]): Process[F,(O,O2)] =
      zipWith(p2)((_,_))

    def to[O2](sink: Sink[F, O]): Process[F, Unit] =
      join { (this zipWith sink)((o, f) => f(o)) }

    def through[O2](p2: Process[F, O => Process[F, O2]]): Process[F, O2] =
      join { (this zipWith p2)((o, f) => f(o)) }
  }

  object Process {
    case class Halt[F[_], O](
      err: Throwable
    ) extends Process[F, O]

    case class Emit[F[_], O](
      head: O,
      tail: Process[F, O]
    ) extends Process[F, O]

    case class Await[F[_], A, O](
      req: F[A],
      recv: Either[Throwable, A] => Process[F, O]
    ) extends Process[F, O]

    def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = Await(req, recv)

    def emit[F[_], O](head: O, tail: Process[F,O] = Halt[F,O](End)): Process[F, O] = Emit(head, tail)

    def eval[F[_], A](a: F[A]): Process[F, A] = await[F, A, A](a) {
      case Right(b) => emit(b, Halt(End))
      case Left(e) => Halt(e)
    }

    def eval_[F[_], A, B](a: F[A]): Process[F, B] = eval[F, A](a).drain[B]

    def lines(filename: String): Process[IO, String] = resource(IO(io.Source.fromFile(filename)))(
      src => {
        lazy val iter = src.getLines()
        def step = if (iter.hasNext) Some(iter.next) else None
        lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
          case None => Halt(End)
          case Some(line) => Emit(line, lines)
        }
        lines
      },
      src => eval_ { IO(src.close()) }
    )

    def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
      val E = java.util.concurrent.Executors.newFixedThreadPool(4)

      @annotation.tailrec
      def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] = cur match {
        case Emit(head, tail) => go(tail, acc :+ head)
        case Halt(End) => acc
        case Halt(err) => throw err
        case Await(req, recv) =>
          val next = try {
            recv(Right(unsafePerformIO(req)(E)))
          } catch { case err: Throwable => recv(Left(err)) }
          go(next, acc)
      }
      try go(src, IndexedSeq())
      finally E.shutdown()
    }

    import java.io.{BufferedReader, FileReader}
    val p: Process[IO, String] = await(IO(new BufferedReader(new FileReader("lines.txt")))) {
      case Right(b) =>
        lazy val next: Process[IO, String] = await(IO(b.readLine())) {
          case Left(e) => await(IO(b.close()))(_ => Halt(e))
          case Right(line) =>
            if (line eq null) Halt(End)
            else emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }

    def resource[R, O](acquire: IO[R])(
      use: R => Process[IO, O],
      release: R => Process[IO, O]
    ): Process[IO, O] = await[IO, R, O](acquire)(r => use(r).onComplete(release(r)))

    case class Is[I]() {
      sealed trait f[X]
      val Get = new f[I] {}
    }

    def Get[I] = Is[I]().Get

    type Process1[I, O] = Process[Is[I]#f, O]

    def await1[I, O](
      recv: I => Process1[I, O],
      fallback: Process1[I, O] = halt1[I, O]
    ): Process1[I, O] = Await(Get[I], (e: Either[Throwable, I]) => e match {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(i) => Try(recv(i))
    })

    def emit1[I, O](h: O, t1: Process1[I, O] = halt1[I, O]): Process1[I, O] = emit(h, t1)

    def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)

    def lift[I, O](f: I => O): Process1[I, O] =
      await1[I, O](i => emit(f(i))).repeat

    def filter[I](f: I => Boolean): Process1[I, I] = await1[I, I](i => if (f(i)) emit(i) else halt1).repeat

    case class T[I, I2]() {
      sealed trait f[X] { def get: Either[I => X, I2 => X] }
      val L = new f[I] { def get = Left(identity) }
      val R = new f[I2] { def get = Right(identity) }
    }

    def L[I, I2] = T[I, I2]().L
    def R[I, I2] = T[I, I2]().R

    type Tee[I, I2, O] = Process[T[I, I2]#f, O]

    def haltT[I, I2, O]: Tee[I, I2, O] = Halt[T[I,I2]#f, O](End)
    def awaitL[I, I2, O](
      recv: I => Tee[I, I2, O],
      fallback: => Tee[I, I2, O] = haltT[I, I2, O]
    ): Tee[I, I2, O] = await[T[I, I2]#f, I, O](L) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

    def awaitR[I, I2, O](
      recv: I => Tee[I, I2, O],
      fallback: => Tee[I, I2, O] = haltT[I, I2, O]
    ): Tee[I, I2, O] = await[T[I, I2]#f, I, O](R) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

    def emitT[I, I2, O](h: O, t1: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = emit(h, t1)

    def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] = awaitL[I, I2, O](i =>
      awaitR(i2 => emitT(f(i, i2)))
    ).repeat

    def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))

    type Sink[F[_], O] = Process[F[_], O => Process[F, Unit]]

    def fileW(file: String, append: Boolean = false): Sink[IO, String] = {
      resource[FileWriter, String => Process[IO, Unit]](
        IO { new FileWriter(file, append)}
      )(
        w => constant { (s: String) => eval[IO, Unit](IO(w.write(s))) },
        w => eval_(IO(w.close()))
      )
    }

    def constant[A](a: A): Process[IO, A] = eval[IO, A](IO(a)).repeat

    def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] = p.flatMap(pp => pp)

    type Channel[F[_], I, O] = Process[F, I => Process[F, O]]

    /**
      * Helper function to safely produce `p`, or gracefully halt
      * with an error if an exception is thrown.
      */
    def Try[F[_],O](p: => Process[F,O]): Process[F,O] =
      try p
      catch { case e: Throwable => Halt(e) }

    case object End extends Exception
    case object Kill extends Exception
  }
}

