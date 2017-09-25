package Chapter7

object ReflectingOnFoldsApp extends App {

  val list = List(1, 2, 3)
  val foldLeftResult = list.foldLeft(List.empty[Int]) { case (acc, x) => x :: acc }
  val foldRightResult = list.foldRight(List.empty[Int]) { case (x, acc) => x :: acc }
  println(s"foldLeftResult: $foldLeftResult")
  println(s"foldRightResult: $foldRightResult")
}
