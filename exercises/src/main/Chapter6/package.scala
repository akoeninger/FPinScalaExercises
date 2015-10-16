package main

import scala.math._

package object Chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, newRNG) = rng.nextInt
      val n = if (i == Int.MinValue) Int.MaxValue else abs(i)

      (n, newRNG)
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
