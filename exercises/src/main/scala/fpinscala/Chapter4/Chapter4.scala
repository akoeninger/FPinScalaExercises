package fpinscala.Chapter4

import fpinscala.Chapter4.Some

object Chapter4 {

  def main(args: Array[String]) {
    println(Option.traverse(List(Some("1"), Some("a"), Some("3")))(s => s))
  }
}
