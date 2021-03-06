package fpinscala.Chapter7

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A, B, C](af: Future[A], bf: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile
    var cache: Option[C] = None

    override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.MILLISECONDS.convert(timeout, unit))

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      af.cancel(mayInterruptIfRunning) || bf.cancel(mayInterruptIfRunning)

    override def isDone: Boolean = cache.isDefined

    private def computeC(timeoutMS: Long): C = {
      val start = System.currentTimeMillis()
      val ar = af.get(timeoutMS, TimeUnit.MILLISECONDS)
      val stop = System.currentTimeMillis()

      val br = bf.get(timeoutMS - (stop - start), TimeUnit.MILLISECONDS)

      val result = f(ar, br)

      cache = Some(result)
      result
    }

    private def compute(timeoutMS: Long): C = cache.fold(computeC(timeoutMS))(identity)
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def flatUnit[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    a.zip(b).flatMap(p => unit(f(p._1, p._2)))

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(a, b)((a, b) => (a, b)), c)((ab, c) => f(ab._1, ab._2, c))

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(
      map2(a, b)((a, b) => (a, b)),
      map2(c, d)((c, d) => (c, d))
    )((ab, cd) => f(ab._1, ab._2, cd._1, cd._2))

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    map2(
      map2(
        map2(a, b)((a, b) => (a, b)),
        map2(c, d)((c, d) => (c, d))
      )((ab, cd) => (ab._1, ab._2, cd._1, cd._2)),
      e
    )((abcd, e) => f(abcd._1, abcd._2, abcd._3, abcd._4, e))

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one,
  // the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism.
  // Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation,
  // and we will discuss this later in the chapter.
  // If using a fixedThreadPool of 1, this could cause deadlocking
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sequence_simple[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil: List[A]))((p, acc) => map2(p, acc)(_ :: _))

  // Forks recursive step in new logical thread, effectively making it tail-recursive
  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = as match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
  }

  // Divides list in half and runs both halves in parallel
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as map asyncF((a: A) => if (f(a)) List(a) else Nil)
    map(sequence(pars))(_.flatten)
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    cond.chooser(bool => if (bool) t else f)

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(cond.map(b => if (b) 0 else 1))(List(t, f))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    n.chooser(choices)

  def choiceN_Book[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val ind = run(es)(n).get
    run(es)(choices(ind)) // IndexOutOfBoundsException potential
  }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = key.chooser(choices)

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => choices(pa.run(es).get).run(es)

  def join[A](a: Par[Par[A]]): Par[A] = es => a(es).get.run(es)

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    a.flatMap(x => x)

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(pa.map(f))



/* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

    def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_, _))

    def map[B](f: (A => B)): Par[B] = Par.map(p)(f)

    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)

    def chooser[B](choices: A => Par[B]): Par[B] = Par.chooser(p)(choices)

    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)

    def map3[B, C, D](b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = Par.map3(p, b, c)(f)

    def map4[B, C, D, E](b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = Par.map4(p, b, c, d)(f)

    def map5[B, C, D, E, F](b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
      Par.map5(p, b, c, d, e)(f)

    def run(s: ExecutorService): Future[A] = p(s)

    def fork: Par[A] = es => es.submit(new Callable[A] {
      def call = p(es).get
    })

    def delay: Par[A] = es => p(es)

    def equal(e: ExecutorService)(p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    def equal(p2: Par[A]): Par[Boolean] = map2(p2)(_ == _)
  }
}
