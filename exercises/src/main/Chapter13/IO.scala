package main.Chapter13

import scala.annotation.tailrec
import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

import main.Chapter11.Monad
import main.Chapter13.IO3.Console.ConsoleIO

object IO0 {
  trait IO { self =>
    def run: Unit
    def ++(io: IO): IO = new IO {
      def run: Unit = { self.run; io.run }
    }
  }
  object IO {
    def empty: IO = new IO { def run: Unit = () }
  }

  /*
    This implementation isn't very useful only a monoid.
    Can't even handle input.
   */
  def fahrenheitToCelsius(f: Double): Double =
     (f - 32) * 5.0/9.0

   // Ordinary code with side effects
   def converter(): Unit = {
     println("Enter a temperature in degrees Fahrenheit: ")
     val d = scala.io.StdIn.readLine().toDouble
     println(fahrenheitToCelsius(d))
   }

   // A pure version is not possible!
   /*
   def converter: IO = {
     val prompt: IO = PrintLine("Enter a temperature in degrees fahrenheit: ")
     // now what ???
   }
   */
}

object IO1 {
  /*
   We need a way for our `IO` actions to yield a result of some
   meaningful type. We do this by adding a type parameter to `IO`,
   which now forms a `Monad`.
  */


  sealed trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] = new IO[B] {
      override def run: B = f(self.run)
    }
    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      override def run: B = f(self.run).run
    }
  }

  object IO extends Monad[IO] {
    override def unit[A](a: => A): IO[A] = new IO[A] {
      override def run: A = a
    }
    override def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = ma flatMap f
    def apply[A](a: => A): IO[A] = unit(a)

    def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO { value = a; a }
      def get: IO[A] = IO { value }
      def modify(f: A => A): IO[A] = get.flatMap(a => set(f(a)))
    }
  }

  def ReadLine: IO[String] = IO { scala.io.StdIn.readLine() }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(IO0.fahrenheitToCelsius(d).toString)
  } yield ()


  /*                         Some other examples                      */

    import IO._ // import all the `IO` combinators that come from `Monad`

    // An `IO[Unit]` that reads a line from the console and echoes it back.
    val echo = ReadLine.flatMap(PrintLine)

    // Parses an `Int` by reading a line from the console.
    val readInt: IO[Int] = ReadLine.map(_.toInt)

    // Parses an `(Int,Int)` by reading two lines from the console.
    val readInts: IO[(Int,Int)] = readInt ** readInt

    // Repeat `converter` 5 times, discarding the results (which are
    // just `Unit`). We can replace `converter` here with any `IO`
    // action we wished to repeat 5 times (ex: `echo` or `readInts`).
    val prompts: IO[Unit] = replicateM_(5)(converter)

    // An `IO[List[String]]` that will read 10 lines from the console and
    // return the list of results.
    val lines: IO[List[String]] = replicateM(10)(ReadLine)

  /*
    Larger example using various monadic combinators. Sample run:
       The Amazing Factorial REPL, v2.0
       q - quit
       <number> - compute the factorial of the given number
       <anything else> - bomb with horrible error
       3
       factorial: 6
       7
       factorial: 5040
       q
   */
    val helpstring = """
    | The Amazing Factorial REPL, v2.0
    | q - quit
    | <number> - compute the factorial of the given number
    | <anything else> - bomb with horrible error
    """.trim.stripMargin

    def factorial(n: Int): IO[Int] = for {
      acc <- ref(1)
      _ <- foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
      result <- acc.get
    } yield result

    val factorialREPL: IO[Unit] = sequence_(
      IO { println(helpstring) },
      doWhile { IO { readLine } } { line =>
        val ok = line != "q"
        when (ok) { for {
          n <- factorial(line.toInt)
          _ <- IO { println("factorial: " + n) }
        } yield () }
      }
    )
}

object IO2a {

  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = Return(a)
    def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f
    def suspend[A](a: => IO[A]) =
      Suspend(() => ()).flatMap(_ => a)
  }
  def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))
  val p = IO.forever(printLine("Still going..."))

  @tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(resume) => resume()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(resume) => run(f(resume))
      case FlatMap(y, g) => run(IO.flatMap(y)(a => IO.flatMap(g(a))(f)))
    }
  }
}

object IO2aTests {
  import IO2a._

  /*
  Pg 240: REPL session has a typo, should be:
  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b)}
  }
  Note: we could write a little helper function to make this nicer:
  def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap { _ => a }
  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => suspend { a(x).flatMap(b) }
  }
   */

  val f: Int => IO[Int] = (i: Int) => Return(i)

  val g: Int => IO[Int] =
    List.fill(10000)(f).foldLeft(f){
      (a: Function1[Int, IO[Int]],
        b: Function1[Int, IO[Int]]) => {
        (x: Int) => IO.suspend(a(x).flatMap(b))
      }
    }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}

object IO2b {
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)
    def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f
    def suspend[A](a: => TailRec[A]) =
      Suspend(() => ()).flatMap(_ => a)
  }
  def printLine(s: String): TailRec[Unit] = Suspend(() => println(s))
  val p = TailRec.forever(printLine("Still going..."))

  @tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(resume) => resume()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r))
      case FlatMap(y, g) => run(TailRec.flatMap(y)(a => TailRec.flatMap(g(a))(f)))
    }
  }
}

object IO2bTests {
  import IO2b._

  val f: Int => TailRec[Int] = (i: Int) => Return(i)

  val g: Int => TailRec[Int] =
    List.fill(10000)(f).foldLeft(f){
      (a: (Int) => TailRec[Int],
        b: (Int) => TailRec[Int]) => {
        (x: Int) => TailRec.suspend(a(x).flatMap(b))
      }
    }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}

object IO2c {
  import main.Chapter7.Nonblocking._

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
    def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  object Async extends Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)
    def flatMap[A,B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
  }

  @tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(resume) => resume
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }
}

object IO3 {
  import main.Chapter7.Nonblocking.Par

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F,B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F,A], f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A) = Return(a)

    override def flatMap[A, B](a: Free[F, A])(f: (A) => Free[F, B]) = FlatMap(a, f)
  }

  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(x) => x
    case Suspend(s) => s()
    case FlatMap(s, f) => s match {
      case Return(a1) => runTrampoline { f(a) }
      case Suspend(r) => runTrampoline { f(r()) }
      case FlatMap(a0, g) => runTrampoline { a0 flatMap { a1 => g(a1) flatMap f } }
    }
  }

  @tailrec
  private def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(Return(a), f) => step(f(a))
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
    case _ => free
  }

  def run[F[_],A](free: Free[F,A])(implicit F: Monad[F]): F[A] = step(free) match {
    case Return(a) => F.unit(a)
    case Suspend(s) => s
    case FlatMap(x, f) => x match {
      case Suspend(r) => F.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible due to step method")
    }
  }

  import main.Chapter7.Nonblocking.Par

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A

    def toReader: ConsoleReader[A]
    def toState: ConsoleState[A]
  }
  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)

    override def toThunk: () => Option[String] = () => run

    override def toReader: ConsoleReader[Option[String]] = ConsoleReader(in => Some(in))

    override def toState: ConsoleState[Option[String]] = ConsoleState { bufs =>
      bufs.in match {
        case Nil => (None, bufs)
        case h :: t => (Some(h), bufs.copy(in = t))
      }
    }

    def run: Option[String] =
      try Some(scala.io.StdIn.readLine())
      catch { case e: Exception => None }
  }
  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))

    override def toThunk: () => Unit = () => println(line)

    override def toReader: ConsoleReader[Unit] = ConsoleReader(s => ())

    override def toState: ConsoleState[Unit] = ConsoleState { bufs => ((), bufs.copy(out = bufs.out :+ line)) }
  }
  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

    val f1: ConsoleIO[Option[String]] = for {
      _ <- printLn("I can only interact with the console.")
      ln <- readLn
    } yield ln
  }

  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): Function0[A] = f.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    override def apply[A](f: Console[A]): Par[A] = f.toPar
  }

  val consoleToReader = new (Console ~> ConsoleReader) {
    override def apply[A](f: Console[A]): ConsoleReader[A] = f.toReader
  }
  val consoleToState = new (Console ~> ConsoleState) {
    override def apply[A](f: Console[A]): ConsoleState[A] = f.toState
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(s) => t(s)
    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("Impossible; `step` eliminates these cases.")
  }

  def runConsoleFunction0[A](a: ConsoleIO[A]): () => A =
    runFree[Console,Function0,A](a)(consoleToFunction0)

  def runConsolePar[A](a: ConsoleIO[A]): Par[A] =
    runFree[Console,Par,A](a)(consoleToPar)

  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console,ConsoleReader,A] (io)(consoleToReader)

  def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] =
    runFree[Console,ConsoleState,A](io)(consoleToState)

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A,B](a: () => A)(f: A => (() => B)) = () => f(a())()
  }
  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](a: Par[A])(f: (A) => Par[B]): Par[B] = Par.fork {
      Par.flatMap(a)(f)
    }
  }

  /*
   The `runConsoleFunction0` implementation is unfortunately not stack safe,
   because it relies of the stack safety of the underlying monad, and the
   `Function0` monad we gave is not stack safe. To see the problem, try
   running: `freeMonad.forever(Console.printLn("Hello"))`.
   */

   // Exercise 4 (optional, hard): Implement `runConsole` using `runFree`,
   // without going through `Par`. Hint: define `translate` using `runFree`.


   def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
     type FreeG[B] = Free[G, B]
     val t = new (F ~> FreeG) {
       override def apply[B](f: F[B]): FreeG[B] = Suspend { fg(f) }
     }
     runFree(f)(t)(freeMonad[G])
   }

   def runConsole[A](a: Free[Console,A]): A = runTrampoline {
     translate(a)(new (Console ~> Function0) {
       override def apply[A](f: Console[A]): () => A = f.toThunk
     })
   }

  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))

    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }
  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      override def unit[A](a: => A): ConsoleReader[A] = ConsoleReader(_ => a)

      override def flatMap[A, B](a: ConsoleReader[A])(f: (A) => ConsoleReader[B]): ConsoleReader[B] =
        a flatMap f
    }
  }
  // TODO: Use TailRec to make this stack safe
  case class Buffers(in: List[String], out: List[String])
  case class ConsoleState[A](run: Buffers => (A, Buffers)) {
    def map[B](f: A => B): ConsoleState[B] = ConsoleState { s =>
      val (a, s1) = run(s)
      (f(a), s1)
    }

    def flatMap[A,B](f: A => ConsoleState[B]): ConsoleState[B] = ConsoleState { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  }
  object ConsoleState {
    implicit val monad: Monad[ConsoleState] = new Monad[ConsoleState] {
      override def flatMap[A, B](a: ConsoleState[A])(f: (A) => ConsoleState[B]): ConsoleState[B] = a flatMap f

      override def unit[A](a: => A): ConsoleState[A] = ConsoleState(bufs => (a, bufs))
    }
  }
}
