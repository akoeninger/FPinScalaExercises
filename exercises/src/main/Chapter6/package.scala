package main

package object Chapter6 {

  def main(args: Array[String]): Unit = {
    println(Candy.simulateMachine(List(Coin, Coin, Turn, Turn, Coin, Turn, Coin, Turn, Turn)).run(Machine(true, 10, 0)))
  }
}
