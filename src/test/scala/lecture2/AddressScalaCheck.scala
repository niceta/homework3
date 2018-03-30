package lecture2

import lecture2.AddressObject.{Address, ConstructAddressException, EmptyNameException}
import org.scalacheck.{Gen, Prop, Properties}

object AddressScalaCheck extends Properties("MyAddressGen") {
  def addressString(address: Address): String = address.toString
  def compare(list: List[Any]): Boolean = AddressObject.listToAddress(list).toString == addressString(list)

  property("AddressList") = Prop.forAll(genAddressList(5)) { xs: List[Any] =>
    println(xs)
    Prop.throws(classOf[EmptyNameException]) {compare(xs)} ||
    Prop.throws(classOf[ConstructAddressException]) {compare(xs)} ||
    compare(xs)
  }

  def genAddressList(maxSize: Int): Gen[List[Any]] = {
    def getGen: Gen[_] = {
      val random = Gen.choose(1, 2)
      if (random.sample.get == 1) Gen.choose(1, 256)
      else Gen.option(Gen.choose(1, 256))
    }
    for {
      street   <- Gen.alphaStr
      house    <- Gen.choose(1, 256)
      apart    <- getGen
      tailSize <- Gen.choose(0, maxSize)
      tail     <- Gen.listOfN(tailSize, Gen.alphaStr)
    } yield List(street, house, apart) ++ tail
  }

  /*property("AddressListInt") = forAll(genAddressListInt(5)) {xs: List[Any] =>
      compare(xs)
    }
    property("AddressListSmall") = forAll(genAddressListSmall(5)) {xs: List[Any] =>
      compare(xs)
    }


  def genAddressListSmall(maxSize: Int): Gen[List[Any]] = {
    for {
      street <- Gen.alphaStr
      house  <- Gen.choose(1, 256)
      tailSize <- Gen.choose(0, maxSize)
      tail     <- Gen.listOfN(tailSize, Gen.alphaStr)
    } yield List(street, house) ++ tail
  }
  def genAddressListInt(maxSize: Int): Gen[List[Any]] = {
    for {
      street   <- Gen.alphaStr
      house    <- Gen.choose(1, 256)
      apart    <- Gen.choose(1, 256)
      tailSize <- Gen.choose(0, maxSize)
      tail     <- Gen.listOfN(tailSize, Gen.alphaStr)
    } yield List(street, house, apart) ++ tail
  }

  def genAddressListOption(maxSize: Int): Gen[List[Any]] = {
    for {
      street   <- Gen.alphaStr
      house    <- Gen.choose(1, 256)
      apart    <- Gen.option(Gen.choose(1, 256))
      tailSize <- Gen.choose(0, maxSize)
      tail     <- Gen.listOfN(tailSize, Gen.alphaStr)
    } yield List(street, house, apart) ++ tail
  }*/

  /*println(genAddressList(3).sample)
  println(genAddressList(3).sample)
  println(genAddressList(3).sample)
  println(genAddressList(3).sample)
  println(genAddressList(3).sample)
  println(genAddressList(3).sample)*/
}
