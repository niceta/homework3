package lecture2

object PermutationObject {
  def myPermutation(l: List[Int]): List[List[Int]] = {
    def permutationList(l: List[Int]): List[List[Int]] = l match {
      case List() => List(List.empty[Int])
      case List(elem) => List(List(elem))
      case list =>
        for {
          i <- List.range(0, list.length)
          p <- permutationList(list.slice(0, i) ++ list.slice(i + 1, list.length))
        } yield list(i) :: p
    }
    permutationList(l).distinct
  }

  def main(args: Array[String]): Unit = {
    println(myPermutation(List(1, 2, 3)))
  }
}
