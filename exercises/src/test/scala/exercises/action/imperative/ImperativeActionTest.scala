package exercises.action.imperative

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.{Failure, Success, Try}

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.imperative.ImperativeActionTest
class ImperativeActionTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("retry when maxAttempt is 0") {
    val result = Try(retry(0)(""))
    assert(result.isFailure)
  }

  test("retry when action fails") {
    var counter = 0
    val error   = new Exception("Boom")

    val result = Try(retry(5) {
      counter += 1
      throw error
    })

    assert(result == Failure(error))
    assert(counter == 5)
  }

  test("retry until action succeeds") {
    var counter = 0
    val result = Try(retry(5) {
      counter += 1
      require(counter >= 3, "Counter is too low")
      "Hello"
    })
    assert(result == Success("Hello"))
    assert(counter == 3)
  }

  test("retryImperative when maxAttempt is 0") {
    val result = Try(retryImperative(0)(""))
    assert(result.isFailure)
  }

  test("retryImperative when action fails") {
    var counter = 0
    val error   = new Exception("Boom")

    val result = Try(retryImperative(5) {
      counter += 1
      throw error
    })

    assert(result == Failure(error))
    assert(counter == 5)
  }

  test("retryImperative until action succeeds") {
    var counter = 0
    val result = Try(retryImperative(5) {
      counter += 1
      require(counter >= 3, "Counter is too low")
      "Hello"
    })
    assert(result == Success("Hello"))
    assert(counter == 3)
  }

  test("onError failure") {
    var counter = 0

    val result = Try(onError(
      action = throw new RuntimeException("Booom"),
      cleanup = _ => counter += 1
    ))

    assert(result.isFailure)
    assert(counter == 1)
  }

  test("onError success") {
    var counter = 0

    val result = Try(onError(
      action = "Hello",
      cleanup = _ => counter += 1
    ))

    assert(result.isSuccess)
    assert(counter == 0)
  }

  test("onError swallow exception from cleanup") {
    val result = Try(onError(
      action = throw new RuntimeException("Action Error"),
      cleanup = _ => throw new RuntimeException("Cleanup Error")
    ))

    assert(result.isFailure)
    assert(result.failed.get.getMessage == "Action Error" )
  }

  test("retry pbt") {
    forAll(Gen.const(3), Gen.chooseNum(1, 5)) { (maxAttempt: Int, numberOfErrors: Int) =>
      var counter = 0
      def myMethod() = {
        if (counter < numberOfErrors) {
          counter += 1
          throw new RuntimeException("Boom")
        } else 42 //meaning of life
      }
      val res = Try(retry(maxAttempt)(myMethod()))

      if (maxAttempt > numberOfErrors)
        assert(res == Success(42))
      else
        assert(res.isFailure)
    }
  }
}
