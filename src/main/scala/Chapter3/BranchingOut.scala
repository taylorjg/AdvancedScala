package Chapter3

import cats.Functor

object BranchingOut extends App {

  import FunctorInstances._
  import TreeSyntax._

  val tree1 = Branch(
    Branch(Leaf(1), Leaf(2)),
    Branch(Leaf(3), Leaf(4))
  )
  val tree2 = tree1.map(n => n * 2)
  println(s"tree2: $tree2")

  println(s"Leaf.map: ${leaf(41).map(_ + 1)}")
  println(s"Branch.map: ${branch(leaf(-8), leaf(8)).map(n => n * n - 22)}")

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

  object TreeSyntax {

    implicit class TreeFunctorOps[A](tree: Tree[A]) {
      def map[B](func: A => B)(implicit functor: Functor[Tree]): Tree[B] =
        functor.map(tree)(func)
    }

    def leaf[A](value: A): Tree[A] = Leaf(value)

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)
  }
}
