import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest._
import org.scalatest.prop.{Generator, GeneratorDrivenPropertyChecks}

import scala.concurrent.Future

case class CustomInt(value: Int) extends AnyVal

class Test extends AsyncWordSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit lazy val customIntGen: Gen[CustomInt] = Arbitrary.arbInt.arbitrary.map(CustomInt.apply)

  implicit def convert[T](gen: Gen[T])(implicit shr: Shrink[T]): Generator[T] =
    Generator.scalaCheckArbitaryGenerator(Arbitrary(gen), shr)

  "do" should {
    "Some condition" in {
      forAll { i: Int =>
        Future(i).map(x => x shouldBe 1) // This compiles
      }
    }

    "Another condition" in {
      val evenInts = for (n <- Gen.choose(-1000, 1000)) yield 2 * n
      forAll (evenInts) { i: Int =>
        Future(i).map(x => x shouldBe 1) // This doesn't compile, can't explicitly specify a generator
      }
    }

    "Yet another condition" in {
      forAll { i: CustomInt =>
        Future(i.value).map(x => x shouldBe 1) // This fails to compile, can't find the implicit customIntGen
      }
    }

    "And yet another condition" in {
      val customEvenInts = for (n <- Gen.choose(-1000, 1000)) yield CustomInt(2 * n)
      forAll (customEvenInts) { i: CustomInt =>
        Future(i.value).map(x => x shouldBe 1) // // This doesn't compile, can't explicitly specify a custom generator
      }
    }
  }
}
