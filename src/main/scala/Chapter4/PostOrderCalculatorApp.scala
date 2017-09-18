package Chapter4

import cats.data.State

object PostOrderCalculatorApp extends App {

  val stateMonad = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    result <- evalOne("+")
  } yield result

  println(s"12+: ${stateMonad.run(List.empty).value}")

  type CalcState[A] = State[List[Int], A]

  private def evalOne(sym: String): CalcState[Int] = {
    State[List[Int], Int] {
      oldStack =>
        def op(f: (Int, Int) => Int): Int = {
          val n1 = oldStack.head
          val n2 = oldStack(1)
          f(n1, n2)
        }
        sym match {
          case "+" =>
            (oldStack.drop(2), op(_ + _))
          case "-" =>
            (oldStack.drop(2), op(_ - _))
          case "*" =>
            (oldStack.drop(2), op(_ * _))
          case "/" =>
            (oldStack.drop(2), op(_ / _))
          case _ =>
            val n = sym.toInt
            (oldStack :+ n, n)
        }
    }
  }
}
