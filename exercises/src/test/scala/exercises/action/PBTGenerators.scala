package exercises.action

import org.scalacheck.{Arbitrary, Gen}

object PBTGenerators {

  val yesNoGenerator: Gen[String] = Gen.oneOf("Y", "N")
  implicit val yesNoArb: Arbitrary[String] = Arbitrary(yesNoGenerator)
}
