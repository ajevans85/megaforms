package megaforms

import Forms._
import Constraints._
import megaforms.validation.Constraint
import minitest.SimpleTestSuite

object FormSuite extends SimpleTestSuite {

  val simpleMapping = tuple(
    "a" -> text,
    "b" -> text
  )

  test("filling a form puts values from a concrete type into the data map") {
    val form = Form(simpleMapping).fill(("1", "2"))

    assertEquals(form.data, Map("a" -> "1", "b" -> "2"))
    // and since the data is valid we also have got a value
    assertEquals(form.value, Some(("1", "2")))

  }

  test("filling a form evaluates field constraints and adds errors") {
    val form = Form(tuple(
      "a" -> integer.verifying(positive),
      "b" -> integer.verifying(positive)
    )).fill((-1, -1))

    assertEquals(form.errors.size, 2)
    assertEquals(form.errors.map(_.path).toSet, Set("a", "b"))
    // and since the form is invalid there is no value
    assertEquals(form.value, None)
  }

  test("filling a form evaluates global constraints and adds errors") {
    val form = Form(
      tuple(
        "a" -> integer,
        "b" -> integer
      ).verifying(Constraint(t => if (t._1 < t._2) Some("a must be less than b") else None)))
       .fill((1, 2))

    assertEquals(form.errors.size, 1)
    assertEquals(form.globalErrors.size, 1)

    // and since the form is invalid there is no value
    assertEquals(form.value, None)
  }

  test("apply on a form instance returns a field abstraction for a path") {
    val form = Form(simpleMapping).fill(("1", "2"))

    val result = form("a")

    assertEquals(result.path, "a")
    assertEquals(result.data, Some("1"))
  }


}
