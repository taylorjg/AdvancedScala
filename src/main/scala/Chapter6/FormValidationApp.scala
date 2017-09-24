package Chapter6

import cats.Cartesian
import cats.data.Validated
import cats.syntax.either._
import cats.instances.list._

import scala.util.Try

object FormValidationApp extends App {

  type FormData = Map[String, String]
  type Errors = List[String]
  type EitherErrorsOr[A] = Either[Errors, A]
  type ValidatedErrorsOr[A] = Validated[Errors, A]

  val dataValid = Map(
    "name" -> "Jon",
    "age" -> "50"
  )

  val dataMissingAge = Map(
    "name" -> "Jon"
  )

  val dataMissingName = Map(
    "age" -> "50"
  )

  val dataBlankName = Map(
    "name" -> "",
    "age" -> "50"
  )

  val dataNonIntAge = Map(
    "name" -> "Jon",
    "age" -> "fifty"
  )

  val dataNegativeAge = Map(
    "name" -> "Jon",
    "age" -> "-50"
  )

  val dataBlankNameAndNegativeAge = Map(
    "name" -> "",
    "age" -> "-50"
  )

  validate(dataValid, "dataValid")
  validate(dataMissingAge, "dataMissingAge")
  validate(dataMissingName, "dataMissingName")
  validate(dataBlankName, "dataBlankName")
  validate(dataNonIntAge, "dataNonIntAge")
  validate(dataNegativeAge, "dataNegativeAge")
  validate(dataBlankNameAndNegativeAge, "dataBlankNameAndNegativeAge")

  private def validate(formData: FormData, label: String): Unit = {
    val validatedName = readName(formData).toValidated
    val validatedAge = readAge(formData).toValidated
    val validatedUser = Cartesian[ValidatedErrorsOr]
      .product(validatedName, validatedAge) map User.tupled
    println(s"$label: $validatedUser")
  }

  private def getValue(formData: FormData,
                       key: String): EitherErrorsOr[String] =
    formData get key match {
      case Some(value) => value.asRight[Errors]
      case None        => List(s"""Value for "$key" missing""").asLeft[String]
    }

  private def parseInt(key: String, s: String): EitherErrorsOr[Int] =
    Try {
      s.toInt
    }.toOption match {
      case Some(value) => value.asRight[Errors]
      case None        => List(s"""Value for "$key" must be an int""").asLeft[Int]
    }

  private def nonBlank(key: String, s: String): EitherErrorsOr[String] =
    if (s.isEmpty) List(s"""Value for "$key" cannot be blank""").asLeft[String]
    else s.asRight[Errors]

  private def nonNegative(key: String, n: Int): EitherErrorsOr[Int] =
    if (n < 0) List(s"""Value for "$key" cannot be negative""").asLeft[Int]
    else n.asRight[Errors]

  private final val Name = "name"
  private final val Age = "age"

  private def readName(formData: FormData): EitherErrorsOr[String] =
    for {
      s1 <- getValue(formData, Name)
      s2 <- nonBlank(Name, s1)
    } yield s2

  private def readAge(formData: FormData): EitherErrorsOr[Int] =
    for {
      s1 <- getValue(formData, Age)
      n1 <- parseInt(Age, s1)
      n2 <- nonNegative(Age, n1)
    } yield n2

  case class User(name: String, age: Int)
}
