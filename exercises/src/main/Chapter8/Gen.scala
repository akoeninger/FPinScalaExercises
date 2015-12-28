package main.Chapter8

import main.Chapter5.Stream
import main.Chapter6._
import main.Chapter7._
import main.Chapter7.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


/** Exercise 8.1 Props for sum: List[Int] => Int
  *
  * 1) sum of list == sum of list.reverse
  * 2) sum of list of same elements == element multiplied by length of list
  * 3) sum of list of sums of list of Ints == sum of flattened list of Ints
  */

/** Exercise 8.2 Properties for max of List[Int]
  *
  * 1) Max for List of single element should be single element
  * 2) Max of list of same value should be that value
  * 3) max of list == max of list reverse
  * 4) max of list that includes Int.MaxValue == Int.MaxValue
  */

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
      case Passed | Proved => p.run(m, n, rng)
      case x => x
    }
  }
  def ||(p: Prop): Prop = Prop {
    (m, n ,rng) => run(m, n, rng) match {
      case Falsified(f, s) => p.tag(f).run(m, n ,rng)
      case x => x
    }
  }

  def tag(msg: String): Prop = Prop {
    (m, n ,rng) => run(m, n, rng) match {
      case Falsified(f, s) => Falsified(s"$msg\n$f", s)
      case x => x
    }
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified: Boolean = false
  }
  case object Proved extends Result {
    def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def randomStream[A](g: Gen[A])(rng: RNG) = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): FailedCase =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n) map { case (a, i) =>
      try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    } find(_.isFalsified) getOrElse Passed
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p =>
        Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }
      ).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(
    p: Prop,
    maxSize: MaxSize = 100,
    testCases: TestCases = 100,
    rNG: RNG = RNG.SimpleRNG(System.currentTimeMillis)
  ): Unit = p.run(maxSize, testCases, rNG) match {
    case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
    case Passed => println(s"+ OK, passed $testCases")
    case Proved => println("+ OK, proved property.")
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(a => f(a)))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => this.listOfN(n))

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)
  def unionViaChooser[A](g1: Gen[A], g2: Gen[A]): Gen[A] = chooser(g1, g2, boolean)

  def weightedViaChooser[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    chooser(g1._1, g2._1, Gen(State(RNG._double).map(prob => prob < g1._2.abs / (g1._2.abs + g2._2.abs))))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(State(RNG._double).map(prob => prob < g1._2.abs / (g1._2.abs + g2._2.abs))).flatMap(bool =>
    if (bool) g1._1 else g2._1
  )

  def chooser[A](g1: Gen[A], g2: Gen[A], bool: Gen[Boolean]): Gen[A] =
    bool.flatMap(b => if (b) g1 else g2)

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))

  val maxProp1 = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  // We specify that every sorted list is either empty, has one element,
  // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
  val sortedProp = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a,b) => a > b }
  }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get
  )

}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(a: Int): Gen[A] = forSize(a)
  def map[B](f: A => B): SGen[B] = SGen(forSize andThen (_ map f))
  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen (_ flatMap f))
}
