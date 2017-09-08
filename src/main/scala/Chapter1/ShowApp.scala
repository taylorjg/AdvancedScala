package Chapter1

import cats.Show
import cats.syntax.show._
import cats.instances.string._
import cats.instances.int._

object ShowApp extends App {

  val erik = Cat("Erik", 21, "smokey-grey tabby")
  println(erik.show)

  final case class Cat(name: String, age: Int, colour: String)

  object Cat {
    implicit val catShow: Show[Cat] =
      Show.show(cat =>
        s"${cat.name.show} is a ${cat.age.show} year-old ${cat.colour.show} cat.")
  }
}
