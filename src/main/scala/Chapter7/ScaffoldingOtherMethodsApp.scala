package Chapter7

import cats.Monoid
import cats.instances.int._

object ScaffoldingOtherMethodsApp extends App {

  val list = List(1, 2, 3)

  println(s"myMap: ${myMap(list)(n => n * n)}")
  println(s"myFlatMap: ${myFlatMap(list)(n => List.fill(n)(n))}")
  println(s"myFilter: ${myFilter(list)(_ > 1)}")
  println(s"mySum: ${mySum(list)}")

  private def myMap[A, B](list: List[A])(f: A => B): List[B] =
    list.foldRight(List.empty[B]) {
      case (a, acc) => f(a) :: acc
    }

  private def myFlatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
    list.foldRight(List.empty[B]) {
      case (a, acc) => f(a) ++ acc
    }

  private def myFilter[A](list: List[A])(p: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) {
      case (a, acc) => if (p(a)) a :: acc else acc
    }

  private def mySum[A: Monoid](list: List[A]): A =
    list.foldRight(Monoid[A].empty)(Monoid[A].combine)
}
