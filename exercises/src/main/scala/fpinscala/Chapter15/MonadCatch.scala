package fpinscala.Chapter15

import language.higherKinds
import fpinscala.Chapter13._

/*
 * A context in which exceptions can be caught and thrown.
 */
trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable, A]]
  def fail[A](t: Throwable): F[A]
}

object MonadCatch {
  implicit def task = new MonadCatch[Task] {
    override def attempt[A](a: Task[A]): Task[Either[Throwable, A]] = a.attempt

    override def fail[A](t: Throwable): Task[A] = Task.fail(t)

    override def unit[A](a: => A): Task[A] = Task.unit(a)

    override def flatMap[A, B](a: Task[A])(f: (A) => Task[B]): Task[B] = a flatMap f
  }
}
