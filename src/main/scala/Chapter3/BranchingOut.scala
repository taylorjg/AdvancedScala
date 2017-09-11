package Chapter3

import cats.Functor

object BranchingOut extends App {

  import FunctorInstances._

  val tree1 = Branch(
    Branch(Leaf(1), Leaf(2)),
    Branch(Leaf(3), Leaf(4))
  )
  val tree2 = treeFunctor.map(tree1)(n => n * 2)
  println(s"tree2: $tree2")

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object FunctorInstances {
    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](value: Tree[A])(func: A => B): Tree[B] =
        value match {
          case Leaf(v) => Leaf(func(v))
          case Branch(l, r) => Branch(map(l)(func), map(r)(func))
        }
    }
  }
}
