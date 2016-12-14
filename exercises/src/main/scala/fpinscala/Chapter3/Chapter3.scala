package fpinscala.Chapter3

object Chapter3 {
  def main(args: Array[String]) {
    println(Tree.sizeByFold(Branch[Int](Leaf(0), Branch(Leaf(1), Leaf(2)))))
  }
}
