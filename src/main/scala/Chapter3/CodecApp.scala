package Chapter3

import scala.util.Try

object CodecApp extends App {

  import CodecInstances._
  import CodecSyntax._

  println(s"encode(Box(123)): ${encode(Box(123))}")
  println(s"""decode[Box[Int]]("123"): ${decode[Box[Int]]("123")}""")

  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): Option[A]
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))
      override def decode(value: String): Option[B] = self.decode(value).map(dec)
    }
  }

  object CodecInstances {

    implicit val intCodec: Codec[Int] = new Codec[Int] {
      override def encode(value: Int): String = value.toString
      override def decode(value: String): Option[Int] = Try(value.toInt).toOption
    }

    implicit val stringCodec: Codec[String] = new Codec[String] {
      override def encode(value: String): String = value
      override def decode(value: String): Option[String] = Some(value)
    }
  }

  object CodecSyntax {

    def encode[A](value: A)(implicit codec: Codec[A]): String =
      codec.encode(value)

    def decode[A](value: String)(implicit codec: Codec[A]): Option[A] =
      codec.decode(value)
  }

  final case class Box[A](value: A)

  object Box {
    implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] =
      codec.imap[Box[A]](Box.apply, _.value)
  }
}
