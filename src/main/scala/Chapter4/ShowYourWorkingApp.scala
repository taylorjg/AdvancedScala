package Chapter4

import cats.data.Writer
import cats.syntax.writer._
import cats.syntax.applicative._
import cats.instances.vector._
//import scala.concurrent._
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.duration._

object ShowYourWorkingApp extends App {

  println(s"factorial(3): ${factorial(3)}")
  println(s"factorialWriter(3).run: ${factorialWriter(3).run}")

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialWriter(n: Int): Logged[Int] = {
    val wans =
      if (n == 0) 1.pure[Logged]
      else factorialWriter(n - 1).map(n * _)
    wans.flatMap(ans => ans.writer(Vector(s"fact $n $ans")))
  }
}
