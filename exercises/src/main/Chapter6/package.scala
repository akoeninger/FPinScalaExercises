package main

import scala.math._

package object Chapter6 {
  trait RNG {
    type Rand[+A] = RNG => (A, RNG)

    def nextInt: (Int, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, newRNG) = rng.nextInt
      (if (i < 0) -(i + 1) else i, newRNG)
    }

    def double(rng: RNG): (Double, RNG) = _double(rng)

    val _double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

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
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def main(args: Array[String]) {

  }
}
