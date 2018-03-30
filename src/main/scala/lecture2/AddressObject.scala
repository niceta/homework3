package lecture2

object AddressObject {
  case class Address(street: String, house: Int, apartment: Option[Int]) {
    override def toString: String = s"улица $street, " +
      s"дом $house${apartment.fold("")(", квартира " + _)}"
  }

  implicit def listToAddress(xs: List[Any]): Address = {
    if (xs.nonEmpty && xs.head.toString == "") throw EmptyNameException(xs)
    xs match {
      case List(str: String, h: Int, ap: Int, _*) => Address(str, h, Some(ap))
      case List(str: String, h: Int, ap: Option[Int], _*) => Address(str, h, ap)
      case List(str: String, h: Int, _*) => Address(str, h, None)
      case _ => throw ConstructAddressException(xs)
    }
  }

  case class ConstructAddressException(list: List[Any]) extends Exception {
    override def toString: String = s"Can't construct Address from list: $list"
  }

  case class EmptyNameException(list: List[Any]) extends Exception {
    override def toString: String = s"Street name is empty in list: $list"
  }

  def printAddress(address: Address): Unit = {
    println(address)
  }
  //Примерчики
  def main(args: Array[String]): Unit = {
    printAddress(List("Ленина", 12, Some(11)))
    printAddress(List("Ленина", 12, 11))
    printAddress(List("Ленина", 12))
    printAddress(List("Ленина", 12, 11, "ASD","GGGGG"))
    printAddress(List("Ленина", 12, "!@#!@#!@#"))
    printAddress(List("", 184, Some(122), "", "", "")) // Тут эксепшн!
    printAddress(List("Ленина"))                       // И тут, если б дошли :)
  }
}


