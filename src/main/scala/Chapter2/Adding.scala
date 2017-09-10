package Chapter2

import cats.Monoid
import cats.instances.int._

object Adding extends App {

  val xs = List(1, 2, 3, 4, 5)
  println(s"add($xs): ${add(xs)}")

  def add(items: List[Int]): Int = {
    val m = Monoid[Int]
    items.foldLeft(m.empty)(m.combine)
  }
}
