package main.Chapter13

import scala.annotation.tailrec

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
    def printLine(s: String): TailRec[Unit] = Suspend(() => Return(println(s)))
    val p = TailRec.forever(printLine("Still going..."))

    @tailrec
    def run[A](io: TailRec[A]): A = io match {
      case Return(a) => a
      case Suspend(resume) => resume()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(resume) => run(f(resume))
        case FlatMap(y, g) => run(TailRec.flatMap(y)(a => TailRec.flatMap(g(a))(f)))
      }
    }
}

object IO2bTests {
  import IO2b._

  val f: Int => TailRec[Int] = (i: Int) => Return(i)

  val g: Int => TailRec[Int] =
    List.fill(10000)(f).foldLeft(f){
      (a: Function1[Int, TailRec[Int]],
        b: Function1[Int, TailRec[Int]]) => {
        (x: Int) => TailRec.suspend(a(x).flatMap(b))
      }
    }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}
