package lecture2
import org.scalatest.FlatSpec

class PermutationSpec extends FlatSpec {
  val testedFunc: List[Int] => List[List[Int]] = PermutationObject.myPermutation

  def compare(actual: List[List[Int]], expected: List[Int]): Boolean = {
    val expectedResult = expected.permutations.toList
    actual.diff(expectedResult).isEmpty && expectedResult.diff(actual).isEmpty
  }

  "My permutation func" should "work right for empty list" in {
    val xs = List.empty[Int]
    assert(compare(testedFunc(xs), xs))
  }

  "My permutation func" should "work right for list with not repetitive elements" in {
    val xs = List(1, 2, 3)
    assert(compare(testedFunc(xs), xs))
  }

  "My permutation func" should "work right for list with repetitive element elements" in {
    val xs = List(1, 2, 2)
    assert(compare(testedFunc(xs), xs))
  }

  "My permutation func" should "work right for list with same elements" in {
    val xs = List(2, 2, 2)
    assert(compare(testedFunc(xs), xs))
  }
}
