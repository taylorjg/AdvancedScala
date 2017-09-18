package Chapter4

import cats.data.Reader
import cats.syntax.applicative._

object HackingReadersApp extends App {

  case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  private val db =
    Db(Map(
         1 -> "dade",
         2 -> "kate",
         3 -> "margo"
       ),
       Map(
         "dade" -> "zerocool",
         "kate" -> "acidburn",
         "margo" -> "secret"
       ))

  println(
    s"""checkLogin(1, "zerocool"): ${checkLogin(1, "zerocool").run(db)}""")
  println(s"""checkLogin(4, "davinci"): ${checkLogin(4, "davinci").run(db)}""")

  private def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  private def checkPassword(
      username: String,
      password: String
  ): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  private def checkLogin(
      userId: Int,
      password: String
  ): DbReader[Boolean] =
    findUsername(userId).flatMap {
      case (Some(username)) => checkPassword(username, password)
      case None             => false.pure[DbReader]
    }
}
