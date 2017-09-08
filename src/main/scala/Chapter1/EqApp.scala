package Chapter1

import cats.Eq
import cats.syntax.eq._
import cats.instances.string._
import cats.instances.int._
import cats.instances.option._

object EqApp extends App {

  val cat1 = Cat("Garfield", 35, "orange and black")
  val cat2 = Cat("Heathcliff", 30, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat1)
  println(cat2 === cat2)
  println(cat1 === cat2)
  println(cat1 =!= cat2)

  println(optionCat1 === optionCat1)
  println(optionCat2 === optionCat2)
  println(optionCat1 === optionCat2)
  println(optionCat1 =!= optionCat2)
  println(optionCat1 === None)
  println(optionCat2 === None)

  final case class Cat(name: String, age: Int, colour: String)

  object Cat {
    implicit val catEqual: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
      cat1.name === cat2.name &&
      cat1.age === cat2.age &&
      cat1.colour === cat2.colour
    }
  }
}
