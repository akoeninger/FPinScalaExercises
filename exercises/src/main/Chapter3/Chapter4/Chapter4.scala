package main.Chapter3.Chapter4


object Chapter4 {
  sealed trait Option[+A]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def main(args: Array[String]) {

  }
}
