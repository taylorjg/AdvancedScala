package Chapter4

import cats.Monad

object BranchingOutFurtherApp extends App {

  val tree1 = branch(leaf(1), leaf(2))
  val tree2 = MonadInstances.treeMonad.flatMap(tree1)(x =>
    branch(leaf(x.toDouble), leaf(x.toDouble * 2)))
  println(s"tree1: $tree1; tree2: $tree2")

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  object MonadInstances {
    implicit def treeMonad: Monad[Tree] = new Monad[Tree] {

      override def pure[A](x: A): Tree[A] = leaf(x)

      override def flatMap[A, B](fa: Tree[A])(f: (A) => Tree[B]): Tree[B] =
        fa match {
          case Leaf(a) =>
            val fb = f(a)
            fb
          case Branch(l, r) =>
            val fbl = flatMap(l)(f)
            val fbr = flatMap(r)(f)
            branch(fbl, fbr)
        }

      override def tailRecM[A, B](a: A)(
          f: (A) => Tree[Either[A, B]]): Tree[B] = {

        def go(tree: Tree[Either[A, B]]): Tree[B] = {
          tree match {
            case Leaf(Left(x))  => tailRecM(x)(f)
            case Leaf(Right(b)) => leaf(b)
            case Branch(l, r) => branch(go(l), go(r))
          }
        }

        f(a) match {
          case Leaf(Left(x))  => tailRecM(x)(f)
          case Leaf(Right(b)) => leaf(b)
          case Branch(l, r) => branch(go(l), go(r))
        }
      }
    }
  }
}
