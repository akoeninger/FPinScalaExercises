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
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

