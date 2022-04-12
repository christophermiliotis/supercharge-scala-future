package exercises.action.imperative

import java.time.{Instant, LocalDate}
import exercises.action.imperative.UserCreationExercises._
import exercises.action.DateGenerator._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import exercises.action.PBTGenerators._

import java.time.format.DateTimeFormatter
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Random, Success, Try}

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.imperative.UserCreationExercisesTest
class UserCreationExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("parses Yes or No responses to a boolean") {
    assert(UserCreationExercises.parseYesNo("Y"))
    assert(!UserCreationExercises.parseYesNo("N"))
    intercept[IllegalArgumentException](UserCreationExercises.parseYesNo("a"))
  }

  test("readSubscribeToMailingList example") {
    val inputs = ListBuffer("N")
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(inputs, outputs)
    val result = readSubscribeToMailingList(console)

    assert(result == false)
    assert(outputs.toList == List("Would you like to subscribe to our mailing list? [Y/N]"))
  }

  test("readSubscribeToMailingList property-based") {
    forAll() { (yesNo: Boolean) =>
      val inputs = ListBuffer(formatYesNo(yesNo))
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(inputs, outputs)
      val result = readSubscribeToMailingList(console)

      assert(result == yesNo)
      assert(outputs.toList == List("Would you like to subscribe to our mailing list? [Y/N]"))
    }
  }

  test("parseAndFormat round-trip property-based") {
    forAll(yesNoGenerator)((input: String) => {
      assert(formatYesNo(parseYesNo(input)) == input)
    })
  }

  test("readSubscribeToMailingList example failure") {
    val console = Console.mock(ListBuffer("Never"), ListBuffer())
    val result = Try(readSubscribeToMailingList(console))

    assert(result.isFailure)
  }

  test("readDateOfBirth example success") {
    val console = Console.mock(ListBuffer("21-07-1986"), ListBuffer())
    val result = readDateOfBirth(console)

    assert(result == LocalDate.of(1986, 7, 21))
  }

  test("readDateOfBirth example failure") {
    val console = Console.mock(ListBuffer("21/07/1986"), ListBuffer())
    val result = Try(readDateOfBirth(console))

    assert(result.isFailure)
  }

  test("readDateOfBirth property-based") {
    forAll(dateGen) { dob =>
      val input = dateOfBirthFormatter.format(dob)
      val inputs = ListBuffer(input)
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(inputs, outputs)
      val result = readDateOfBirth(console)

      assert(result == dob)
    }
  }

  test("readUser example") {
    val inputs = ListBuffer("Eda", "2001-03-18", "hello", "18-03-2001", "Never", "y", "Y")
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(inputs, outputs)
    val now = Instant.now()
    val constantClock = Clock.constant(now)
    val result = readUser(console, constantClock)

    val expected = User(
      name = "Eda",
      dateOfBirth = LocalDate.of(2001, 3, 18),
      subscribedToMailingList = true,
      createdAt = now
    )

    assert(result == expected)
  }

  test("readUser pbt") {
    forAll(Gen.alphaStr, dateGen, Gen.oneOf(List(true, false)), instantGen) { (name: String, dob: LocalDate, yesNo: Boolean, now: Instant) =>
      val inputs = ListBuffer(name, dob.format(dateOfBirthFormatter), formatYesNo(yesNo))
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(inputs, outputs)
      val constantClock = Clock.constant(now)

      val expected = User(name, dob, yesNo, now)
      val result = readUser(console, constantClock)
      assert(result == expected)
    }
  }

  //////////////////////////////////////////////
  // PART 2: Error handling
  //////////////////////////////////////////////

  test("readSubscribeToMailingListRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result = Try(readSubscribeToMailingListRetry(console, maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readSubscribeToMailingListRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never", "N"), outputs)
    val result = readSubscribeToMailingListRetry(console, maxAttempt = 2)

    assert(!result)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No"""",
        "Would you like to subscribe to our mailing list? [Y/N]"
      )
    )
  }

  test("readSubscribeToMailingListRetry example invalid input") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never"), outputs)
    val result = Try(readSubscribeToMailingListRetry(console, maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No""""
      )
    )

    // check that the error message is the same as `readSubscribeToMailingList`
    val console2 = Console.mock(ListBuffer("Never"), ListBuffer.empty[String])
    val result2 = Try(readSubscribeToMailingList(console2))
    assert(result.failed.get.getMessage == result2.failed.get.getMessage)
  }

  test("readDateOfBirthRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result = Try(readSubscribeToMailingListRetry(console, maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readDateOfBirthRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("July 21st 1986", "21-07-1986"), outputs)
    val result = readDateOfBirthRetry(console, maxAttempt = 2)

    assert(result == LocalDate.of(1986, 7, 21))
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001""",
        """What's your date of birth? [dd-mm-yyyy]"""
      )
    )
  }

  test("readDateOfBirthRetry example failure") {
    val outputs = ListBuffer.empty[String]
    val invalidAttempt = "July 21st 1986"
    val console = Console.mock(ListBuffer(invalidAttempt), outputs)
    val result = Try(readDateOfBirthRetry(console, maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001"""
      )
    )
  }

  test("readDateOfBirthRetry pbt") {
    forAll(
      Gen.listOfN(Random.nextInt(20), invalidDateTimeStrings),
      validDateTimeStrings,
      Gen.chooseNum(1, 10)) { (invalidDateInputs: List[String], validInput: String, retries: Int) =>
      val mixList = invalidDateInputs :+ validInput
      val console = Console.mock(mixList.to(ListBuffer), ListBuffer.empty)

      val result = Try(readDateOfBirthRetry(console, retries))
      if (invalidDateInputs.size >= retries) assert(result.isFailure)
      else assert(result.isSuccess)
    }
  }

}
