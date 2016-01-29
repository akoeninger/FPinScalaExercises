package main.Chapter4


object Chapter4 {

  def main(args: Array[String]) {
    println(Option.traverse(List(Some("1"), Some("a"), Some("3")))(s => s))
  }
}
