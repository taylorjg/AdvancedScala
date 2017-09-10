package Chapter2

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}

object Truth extends App {

  {
    val m = MonoidInstances.booleanAndMonoid
    println(s"Should be false: ${m.combine(false, false)}")
    println(s"Should be false: ${m.combine(true, false)}")
    println(s"Should be false: ${m.combine(false, true)}")
    println(s"Should be true: ${m.combine(true, true)}")
    println(s"Should be true: ${m.combine(true, m.combine(true, true))}")
    println(s"Should be false: ${m.combine(true, m.combine(true, false))}")
    println
  }

  {
    val m = MonoidInstances.booleanOrMonoid
    println(s"Should be false: ${m.combine(false, false)}")
    println(s"Should be true: ${m.combine(true, false)}")
    println(s"Should be true: ${m.combine(false, true)}")
    println(s"Should be true: ${m.combine(true, true)}")
    println(s"Should be true: ${m.combine(true, m.combine(true, true))}")
    println(s"Should be true: ${m.combine(true, m.combine(true, false))}")
    println(s"Should be false: ${m.combine(false, m.combine(false, false))}")
    println
  }

  {
    val m = MonoidInstances.booleanXorMonoid
    println(s"Should be false: ${m.combine(false, false)}")
    println(s"Should be true: ${m.combine(true, false)}")
    println(s"Should be true: ${m.combine(false, true)}")
    println(s"Should be false: ${m.combine(true, true)}")
    println(s"Should be true: ${m.combine(true, m.combine(true, true))}")
    println(s"Should be false: ${m.combine(true, m.combine(true, false))}")
    println(s"Should be true: ${m.combine(false, m.combine(true, false))}")
    println
  }

  {
    val m = MonoidInstances.booleanXNorMonoid
    println(s"Should be true: ${m.combine(false, false)}")
    println(s"Should be false: ${m.combine(true, false)}")
    println(s"Should be false: ${m.combine(false, true)}")
    println(s"Should be true: ${m.combine(true, true)}")
    println(s"Should be true: ${m.combine(true, m.combine(true, true))}")
    println(s"Should be false: ${m.combine(true, m.combine(true, false))}")
    println(s"Should be true: ${m.combine(false, m.combine(true, false))}")
    println
  }

  object MonoidInstances {

    implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
      override val empty: Boolean = false
    }

    implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x || y
      override val empty: Boolean = true
    }

    implicit val booleanXorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x != y
      override val empty: Boolean = false
    }

    implicit val booleanXNorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x == y
      override val empty: Boolean = true
    }
  }
}
