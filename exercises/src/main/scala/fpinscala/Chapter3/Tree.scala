package main.Chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(v) ⇒ 1
    case Branch(l, r) ⇒ 1 + size(l) + size(r)
  }

  def sizeByFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((l, r) => 1 + l + r)

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) ⇒ v
    case Branch(l, r) ⇒ maximum(l) max maximum(r)
  }

  def maxViaFold(tree: Tree[Int]): Int =
    fold(tree)(v => v)((l, r) => l max r)

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) ⇒ 0
    case Branch(l, r) ⇒ 1 + (depth(l) max depth(r))
  }

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((l, r) => 1 + (l max r))

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) ⇒ Leaf(f(v))
    case Branch(l, r) ⇒ Branch(map(l)(f), map(r)(f))
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  /**
    * f: A => B handles the Leaf case to transform the terminal node from A to B
    * g: (B, B) => B handles transforming the Branch cases after their values have been transformed from A => B
    */
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}