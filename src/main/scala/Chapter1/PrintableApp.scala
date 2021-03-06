package Chapter1

object PrintableApp extends App {

  import PrintableInstances._
  import PrintableSyntax._

  Printable.print("Hello")
  Printable.print(42)
  val erik = Cat("Erik", 21, "smokey-grey tabby")
  Printable.print(erik)
  erik.print

  trait Printable[A] {
    def format(value: A): String
  }

  object PrintableInstances {
    implicit val stringPrintable: Printable[String] = (value: String) => value
    implicit val intPrintable: Printable[Int] = (value: Int) => value.toString
  }

  object Printable {
    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
    def print[A](value: A)(implicit p: Printable[A]): Unit =
      println(p.format(value))
  }

  object PrintableSyntax {
    implicit class PrintOps[A](value: A) {
      def format(implicit p: Printable[A]): String = p.format(value)
      def print(implicit p: Printable[A]): Unit = println(p.format(value))
    }
  }

  final case class Cat(name: String, age: Int, colour: String)

  object Cat {
    implicit val catPrintable: Printable[Cat] = (cat: Cat) =>
      s"${cat.name} is a ${cat.age} year-old ${cat.colour} cat."
  }
}
