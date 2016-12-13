package main.Chapter15

import language.{implicitConversions, higherKinds, postfixOps}

import main.Chapter13.{IO, Monad, Free, unsafePerformIO}

object ImperativeAndLazyIO {
  def linesGt40k(filename: String): IO[Boolean] = IO {
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines()
      while (count <= 40000 && lines.hasNext) {
        lines.next()
        count += 1
      }
      count > 40000
    }
    finally src.close()
  }

  /*
The above code is rather low-level, and it's not compositional,
either. Consider the following scenarios:
* Check whether the number of _nonempty_ lines in the file exceeds
40,000
* Find a line index before 40,000 where the first letter of
consecutive lines spells out `"abracadabra"`.
We cannot just compose our existing implementation with some
other combinator(s) to implement these tasks. Our implementation is
a monolithic loop, and we must modify this loop directly if we want
to change its behavior.
Now imagine if we had a `Stream[String]` for the lines of the file
and we could assemble functionality using all the `Stream` functions
we know and love.
   */

object Examples {
val lines: Stream[String] = sys.error("defined elsewhere")
val ex1 = lines.zipWithIndex.exists(_._2 + 1 >= 40000)
val ex2 = lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
val ex3 = lines.take(40000).map(_.head).indexOfSlice("abracadabra".toList)
}

}
