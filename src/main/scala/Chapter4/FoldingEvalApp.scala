package Chapter4

import cats.Eval

object FoldingEvalApp extends App {

  val xs1 = (1L to 5000L).toList
  val xs2 = (1L to 10000L).toList

  println(s"foldRight(xs1, 0L)(_ + _): ${foldRight(xs1, 0L)(_ + _)}")
  println(s"foldRightEval(xs2, 0L)(_ + _).value: ${foldRightEval(xs2, 0L)(_ + _).value}")

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRightEval[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(foldRightEval(tail, acc)(fn)).map(fn(head, _))
      case Nil =>
        Eval.now(acc)
    }
}
