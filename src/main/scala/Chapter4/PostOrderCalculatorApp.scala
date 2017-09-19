package Chapter4

import cats.data.State

object PostOrderCalculatorApp extends App {

  val program1 = for {
    _ <- evalOne("10")
    _ <- evalOne("2")
    result <- evalOne("-")
  } yield result
  println(s"program1: ${program1.run(Nil).value}")

  val program2 = evalAll(List("1", "2", "+", "3", "*"))
  println(s"program2: ${program2.run(Nil).value}")

  type CalcState[A] = State[List[Int], A]

  private def evalAll(input: List[String]): CalcState[Int] =
    input match {
      case hd :: Nil => evalOne(hd)
      case hd :: tl => evalOne(hd).flatMap(_ => evalAll(tl))
      case _ =>
        State[List[Int], Int] { oldStack =>
          (oldStack, 0)
        }
    }

  private def evalOne(sym: String): CalcState[Int] = {
    State[List[Int], Int] { oldStack =>
      def op(f: (Int, Int) => Int): (List[Int], Int) = {
        val (n2 :: n1 :: rest) = oldStack
        val r = f(n1, n2)
        (r :: rest, r)
      }
      sym match {
        case "+" => op(_ + _)
        case "-" => op(_ - _)
        case "*" => op(_ * _)
        case "/" => op(_ / _)
        case _ =>
          val r = sym.toInt
          (r :: oldStack, r)
      }
    }
  }
}
