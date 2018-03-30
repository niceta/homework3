package lecture2

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object PermutationScalaCheck extends Properties("MyGen") {

  def genPermutationList(maxSize: Int, g: Gen[Int] = Gen.chooseNum(1, 10)): Gen[List[Int]] = {
    Gen.choose(0, maxSize) flatMap { sz => Gen.listOfN(sz, g) }
  }

  def compare(actual: List[List[Int]], expected: List[Int]): Boolean = {
    val expectedResult = expected.permutations.toList
    actual.diff(expectedResult).isEmpty && expectedResult.diff(actual).isEmpty
  }

  property("GenPermList") = forAll(genPermutationList(7)) { (xs: List[Int]) =>
    println(xs)
    compare(PermutationObject.myPermutation(xs), xs)}
}
