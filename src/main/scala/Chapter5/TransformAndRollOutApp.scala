package Chapter5

import cats.data.EitherT

import scala.concurrent.Await
import cats.instances.future._
import cats.syntax.applicative._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TransformAndRollOutApp extends App {

  type Response[A] = EitherT[Future, String, A]

  private final val PowerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  show(s"""getPowerLevel("Jazz")""", getPowerLevel("Jazz"))
  show(s"""getPowerLevel("Nobby")""", getPowerLevel("Nobby"))

  show(s"""canSpecialMove("Jazz", "Hot Rod")""", canSpecialMove("Jazz", "Hot Rod"))
  show(s"""canSpecialMove("Jazz", "Bumblebee")""", canSpecialMove("Jazz", "Bumblebee"))
  show(s"""canSpecialMove("Jazz", "Nobby")""", canSpecialMove("Jazz", "Nobby"))

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))

  private def show[A](label: String, f: => Response[A]): Unit = {
    val result = Await.result(f.value, 1.second)
    println(s"$label: $result}")
  }

  private def getPowerLevel(autobot: String): Response[Int] =
    PowerLevels.get(autobot) match {
      case Some(powerLevel) =>
        powerLevel.pure[Response]
      case None =>
        EitherT.left[Int](Future(s"Failed to find power level for $autobot"))
    }

  private def canSpecialMove(
      ally1: String,
      ally2: String
  ): Response[Boolean] =
    for {
      x <- getPowerLevel(ally1)
      y <- getPowerLevel(ally2)
    } yield x + y > 15

  private def tacticalReport(
      ally1: String,
      ally2: String
  ): String = {
    val v1 = canSpecialMove(ally1, ally2)
    val v2 = v1.value
    val v3 = v2.map { e =>
      e.map(b =>
        if (b) s"$ally1 and $ally2 are ready to roll out!"
        else s"$ally1 and $ally2 need a recharge.")
    }
    val v4 = Await.result(v3, 1.second)
    val v5 = v4 match {
      case Left(s) => s
      case Right(s) => s
    }
    v5
  }
}
