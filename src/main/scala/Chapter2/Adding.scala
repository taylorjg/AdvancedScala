package Chapter2

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._

object Adding extends App {

  val xs1 = List(1, 2, 3, 4, 5)
  println(s"add($xs1): ${add(xs1)}")

  val xs2 = xs1.map(_.some)
  println(s"add($xs2): ${add(xs2)}")

  import MonoidInstances.orderMonoid
  val orders = List(Order(100, 2), Order(500, 5))
  println(s"add($orders): ${add(orders)}")

  def add[A](items: List[A])(implicit m: Monoid[A]): A = {
    items.foldLeft(m.empty)(m.combine)
  }

  case class Order(totalCost: Double, quantity: Double)

  object MonoidInstances {
    implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
      override def combine(x: Order, y: Order) =
        Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
      override def empty = Order(0, 0)
    }
  }
}
