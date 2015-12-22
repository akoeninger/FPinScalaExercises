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

