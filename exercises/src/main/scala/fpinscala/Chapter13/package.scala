package main

import language.higherKinds
package object Chapter13 {
  import main.scala.Chapter7.Nonblocking._

  type IO[A] = IO3.IO[A]
  type Free[F[_], A] = IO3.Free[F, A]

  implicit val ioMonad = IO3.freeMonad[Par]

  def IO[A](a: => A): IO[A] = IO3.IO[A](a)

  def now[A](a: A): IO[A] = IO3.Return(a)

  def fork[A](a: => IO[A]): IO[A] = par(Par.lazyUnit(())).flatMap(_ => a)

  def forkUnit[A](a: => A): IO[A] = fork(now(a))

  def delay[A](a: => A): IO[A] = now(()).flatMap(_ => now(a))

  def par[A](a: Par[A]): IO[A] = IO3.Suspend(a)

  def async[A](cb: ((A => Unit) => Unit)): IO[A] = fork(par(Par.async(cb)))

  def Return[A](a: A): IO[A] = IO3.Return[Par, A](a)

  import java.util.concurrent.ExecutorService
  def unsafePerformIO[A](io: IO[A])(implicit E: ExecutorService): A = Par.run(E) {
    IO3.run(io)(IO3.parMonad)
  }
}
