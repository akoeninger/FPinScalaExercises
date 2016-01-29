package main

package object Chapter5 {

  def main(args: Array[String]): Unit = {
    println(Stream(2,3,4).hasSubsequence(Stream(3,4)))
  }
}
