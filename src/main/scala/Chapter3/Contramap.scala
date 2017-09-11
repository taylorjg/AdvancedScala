package Chapter3

object Contramap extends App {

  import PrintableInstances._
  import PrintableSyntax._

  val a = 42
  val s = a.format
  println(s"a: $a; s: $s")
  println

  val doublePrintable1 = intPrintable.contramap[Double](_.toInt)
  println(s"p2.format(42.1): ${doublePrintable1.format(42.1)}")
  println(s"p2.format(42.9): ${doublePrintable1.format(42.9)}")
  println

  val doublePrintable2 = a.contramap[Double](_.toInt)
  println(s"p2.format(42.1): ${doublePrintable2.format(42.1)}")
  println(s"p2.format(42.9): ${doublePrintable2.format(42.9)}")

  trait Printable[A] {
    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] =
      (value: B) => format(func(value))
  }

  object PrintableInstances {
    implicit val stringPrintable: Printable[String] = (value: String) => value
    implicit val intPrintable: Printable[Int] = (value: Int) => value.toString
  }

  object PrintableSyntax {
    implicit class PrintOps[A](value: A) {
      def format(implicit p: Printable[A]): String =
        p format value
      def contramap[B](func: B => A)(implicit p: Printable[A]): Printable[B] =
        p contramap func
    }
  }
}
