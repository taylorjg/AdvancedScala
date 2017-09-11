package Chapter3

object BoxContramap extends App {

  import PrintableInstances._
  import Printable._

  println(format("Hello"))
  println(format(true))
  println(format(Box("hello world")))
  println(format(Box(true)))

  trait Printable[A] {
    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] =
      (value: B) => format(func(value))
  }

  object PrintableInstances {

    implicit val stringPrintable: Printable[String] =
      (value: String) => s""""$value""""

    implicit val booleanPrintable: Printable[Boolean] =
      (value: Boolean) => if (value) "yes" else "no"
  }

  object Printable {
    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
  }

  final case class Box[A](value: A)

  object Box {

    import PrintableInstances._

    implicit val boxStringPrintable: Printable[Box[String]] =
      stringPrintable.contramap[Box[String]](_.value)

    implicit val boxBooleanPrintable: Printable[Box[Boolean]] =
      booleanPrintable.contramap[Box[Boolean]](_.value)
  }
}
