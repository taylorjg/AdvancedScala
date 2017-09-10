package Chapter2

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._

object Adding extends App {

  val xs1 = List(1, 2, 3, 4, 5)
  val xs2 = xs1.map(_.some)
  println(s"add($xs1): ${add(xs1)}")
  println(s"add($xs2): ${add(xs2)}")

  def add[A](items: List[A])(implicit m: Monoid[A]): A = {
    items.foldLeft(m.empty)(m.combine)
  }
}
