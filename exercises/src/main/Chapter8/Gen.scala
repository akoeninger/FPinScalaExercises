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
trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop = ???
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(a => f(a)))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n =>
    Gen.listOfN(n, this)
  )
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
}

trait SGen[+A] {

}

