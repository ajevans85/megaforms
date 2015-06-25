package megaforms.validation

import megaforms.validation.Constraints._
import minitest.SimpleTestSuite

object ConstraintsSuite extends SimpleTestSuite {

  test("no validation always is ok") {
    assertEquals(noValidation[String].validate("anything goes"), None)
  }

  test("max gives warning if larger") {
    assert(max(5).validate(6).isDefined)
  }

  test("max does not give warning less or equals") {
    assert(max(5).validate(5).isEmpty)
    assert(max(5).validate(4).isEmpty)
  }

  test("min gives warning if smaller") {
    assert(min(2).validate(1).isDefined)
  }

  test("min does not give warning if equal or larger") {
    assert(min(2).validate(2).isEmpty)
    assert(min(2).validate(3).isEmpty)
  }

  test("between warns for value below min") {
    assert(between(2, 5).validate(1).isDefined)
  }

  test("between does not warn for value equal to min") {
    assert(between(2, 5).validate(2).isEmpty)
  }

  test("between does not warn for value equal to max") {
    assert(between(2, 5).validate(5).isEmpty)
  }

  test("between warns for value larger than max") {
    assert(between(2, 5).validate(6).isDefined)
  }

  test("positive requires value to be positive") {
    assert(positive[Int].validate(-1).isDefined)
    assert(positive[Int].validate(0).isEmpty)
  }

  test("nonEmptyText gives warning on empty text") {
    assert(nonEmptyText.validate("").isDefined)
    assert(nonEmptyText.validate(" ").isDefined)
  }

  test("noneEmptyText is ok as soon as there is text") {
    assert(nonEmptyText.validate("a").isEmpty)
  }

  test("maxLength gives warning if text is longer than limit") {
    assert(maxLength(5).validate("abcdef").isDefined)
  }

  test("maxLength does not give warning if text is within limit") {
    assert(maxLength(5).validate("abcde").isEmpty)
  }

  test("regex gives error if regex does not match") {
    assert(regex("""\d+""", "not only digits").validate("a").isDefined)
  }

  test("regex does not give error if regex does match") {
    assert(regex("""\d+""", "not only digits").validate("23").isEmpty)
  }

}
