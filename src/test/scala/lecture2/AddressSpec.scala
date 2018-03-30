package lecture2

import lecture2.AddressObject.{Address, ConstructAddressException, EmptyNameException}
import org.scalatest.FlatSpec

class AddressSpec extends FlatSpec {

  def addressString(address: Address): String = address.toString
  def compare(list: List[Any]): Boolean = AddressObject.listToAddress(list).toString == addressString(list)

  "My cast-function" should " give an exception for list with empty street name: " in {
    intercept[EmptyNameException] {
      addressString(List("", 252, Some(60)))
    }
  }

  "My cast-function" should " give an exception for empty list:" in {
    intercept[ConstructAddressException] {
      addressString(List())
    }
  }

  "My cast-function" should " give an exception for list of 1 elem:" in {
    intercept[ConstructAddressException] {
      addressString(List(2))
    }
  }

  "My cast-function" should " work right for list of 3 elem:" in {
    val s = List("Ленина", 12, 11)
    assert(compare(s))
  }

  "My cast-function" should " work right for list of 3 elem with option:" in {
    val s = List("Ленина", 12, Some(11))
    assert(compare(s))
  }

  "My cast-function" should " work right for list of 2 elem:" in {
    val s = List("Ленина", 12)
    assert(compare(s))
  }

  "My cast-function" should " work right for list of more than 3 elem:" in {
    val s = List("Ленина", 12, 11, "dafa", "#&@^^#", 234, 22)
    assert(compare(s))
  }

  "My cast-function" should " work right for list of more than 2 elem:" in {
    val s = List("Ленина", 12, "dafa", "#&@^^#", 34, 21, "24")
    assert(compare(s))
  }
}
