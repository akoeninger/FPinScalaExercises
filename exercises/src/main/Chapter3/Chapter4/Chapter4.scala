package main.Chapter3.Chapter4


object Chapter4 {
  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  sealed trait Option[+A] {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f) getOrElse None

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this map(Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] =
      if (map(f) getOrElse false) this
      else None

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap { m =>
        mean(xs map (x => math.pow(x - m, 2)))
      }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap { a1 =>
    b map { b1 =>
      f(a1, b1)
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      a match {
        case Nil => Some(Nil)
        case h :: t =>
          h match {
            case None => None
            case Some(v) => sequence(t) map (v :: _)
          }
      }
    }
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail => head flatMap { h =>
      sequence_1(tail).map(h :: _)
    }
  }

  def sequence_book[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def main(args: Array[String]) {
    println(Option.sequence(List(Some(1), Some(2), None, Some(3))))
  }
}
