package main.Chapter4


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

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(a1 => a1)
    }

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
      aa ← a
      bb ← b
    } yield f(aa, bb)

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail => head flatMap { h =>
      sequence_1(tail).map(h :: _)
    }
  }

  def sequence_book[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil)) { (x, y) =>
      y flatMap { t =>
        f(x).map(_ :: t)
      }
    }
  }
}

  sealed trait Either[+E, +A] {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty left")
      else
        Right(xs.sum / xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e) }

    def Try[AA >: A](a: => AA): Either[Exception, AA] =
      try Right(a)
      catch { case e: Exception => Left(e) }


    def map[B](f: A => B): Either[E, B] = this match {
      case Right(v) => Right(f(v))
      case Left(e) => Left(e)
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(v) => f(v)
      case Left(e) => Left(e)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(_) => this
      case Left(_) => b
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
      a ← this
      bb ← b
    } yield f(a, bb)

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      es.foldRight[Either[E, List[A]]](Right(Nil))((e, eAcc) =>
        e.map2(eAcc)(_ :: _)
      )

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldRight[Either[E, List[B]]](Right(Nil))( (x, acc) =>
        f(x).map2(acc)(_ :: _)
      )

  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]


  def main(args: Array[String]) {
    println(Option.traverse(List(Some("1"), Some("a"), Some("3")))(s => s))
  }
}
