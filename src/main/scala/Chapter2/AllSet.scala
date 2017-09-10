package Chapter2

object AllSet extends App {

  val s1 = Set(1, 2, 3, 4)
  val s2 = Set(3, 4, 5, 6)
  val s3 = Set(4, 5, 6, 7, 8)

  {
    val m = MonoidInstances.setUnionMonoid
    println(s"List(1, 2, 3, 4): ${m.combine(s1, m.empty).toList.sorted}")
    println(s"List(1, 2, 3, 4): ${m.combine(m.empty, s1).toList.sorted}")
    println(s"List(1, 2, 3, 4, 5, 6): ${m.combine(s1, s2).toList.sorted}")
    println(s"List(1, 2, 3, 4, 5, 6, 7, 8): ${m.combine(s3, m.combine(s1, s2)).toList.sorted}")
  }

  {
    val m = MonoidInstances.setIntersectMonoid
    println(s"List(1, 2, 3, 4): ${m.combine(s1, m.empty).toList.sorted}")
    println(s"List(1, 2, 3, 4): ${m.combine(m.empty, s1).toList.sorted}")
    println(s"List(3, 4): ${m.combine(s1, s2).toList.sorted}")
    println(s"List(4): ${m.combine(s3, m.combine(s1, s2)).toList.sorted}")
  }

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

  object MonoidInstances {

    implicit val setUnionMonoid: Monoid[Set[Int]] = new Monoid[Set[Int]] {
      override def combine(x: Set[Int], y: Set[Int]): Set[Int] = x union y
      override def empty: Set[Int] = Set.empty
    }

    implicit val setIntersectMonoid: Monoid[Set[Int]] = new Monoid[Set[Int]] {
      override def combine(x: Set[Int], y: Set[Int]): Set[Int] = x intersect y
      override def empty: Set[Int] = Set.empty
    }
  }
}
