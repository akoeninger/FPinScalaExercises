package main.Chapter6

import scala.math._



case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.reverse.foldLeft(unit[S, List[A]](List()))((a, acc) => acc.map2(a)(_ :: _))


  def modify[S](f: S => S): State[S, Unit] = for {
    s ← get
    _ ← set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

trait RNG {
  type Rand[+A] = State[RNG, A]
  type State[S, +A] = S => (A, S)


  def nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  /*def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }*/

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, newRNG) = rng.nextInt
    (if (i < 0) -(i + 1) else i, newRNG)
  }

  val double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = rng1.double(rng1)

    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)

    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, r: RNG, list: List[Int]): (List[Int], RNG) = {
      if (c == 0)
        (list, r)
      else {
        val (i, r1) = r.nextInt
        go(c - 1, r1, i :: list)
      }
    }

    go(count, rng, Nil)
  }

  def initsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a =>
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  )


  /*def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }*/

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldLeft((Nil: List[A], rng))((acc, a) => {
      val (i, r1) = a(acc._2)
      (i :: acc._1, r1)
    })
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, frng) = f(rng)
    g(a)(frng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(a => {
    val mod = a % n
    if (a + (n - 1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThan(n)
  })
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  private def empty: Boolean = candies == 0

  def input(i: Input): Machine = i match {
    case _ if empty => this
    case Coin if !locked => this
    case Coin => Machine(false, candies, coins + 1)
    case Turn if locked => this
    case Turn => Machine(true, candies - 1, coins)
  }
}

object Candy {
  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ ← sequence(inputs.map(i => State.modify((s: Machine) => s.input(i))))
      s ← get
    } yield (s.coins, s.candies)
  }
}

package object Chapter6 {


  def main(args: Array[String]): Unit = {
    println(Candy.simulateMachine(List(Coin, Coin, Turn, Turn, Coin, Turn, Coin, Turn, Turn)).run(Machine(true, 10, 0)))
  }
}
