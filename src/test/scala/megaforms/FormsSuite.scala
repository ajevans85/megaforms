package megaforms

import Forms._
import minitest.SimpleTestSuite

object FormsSuite extends SimpleTestSuite {

  // since these are the public api, we test here instead of per class under mapping

  test("integer field mapping works") {
    val result = integer.unmap(integer.map(2))
    assertEquals(result, Right(2))
  }

  test("text field mapping works") {
    val result = text.unmap(text.map("woho"))
    assertEquals(result, Right("woho"))
  }

  test("boolean mapping works") {
    val mapped = boolean.map(true)
    // does not actually map to a string, but keeps the boolean
    assertEquals(mapped, Map("" -> true))
    val result = boolean.unmap(mapped)
    assertEquals(result, Right(true))
  }

  test("tuple 2 mapping works") {
    val tuple2Mapping = tuple("a" -> keep[Int], "b" -> keep[Int])
    val result = tuple2Mapping.unmap(tuple2Mapping.map((1,2)))
    assertEquals(result, Right((1,2)))
  }

  test("tuple 3 mapping works") {
    val tuple2Mapping = tuple("a" -> keep[Int], "b" -> keep[Int], "c" -> keep[Int])
    val result = tuple2Mapping.unmap(tuple2Mapping.map((1,2,3)))
    assertEquals(result, Right((1,2,3)))
  }

  test("the transform mapping works") {
    val mapping = text.transform[Int](text => text.length, length => length.toString)
    val mapped = mapping.map(5)
    assertEquals(mapped, Map("" -> "5"))
    val result = mapping.unmap(mapped)
    assertEquals(result, Right(1))
  }

  test("the optional mapping works for some") {
    val mapping = optional(text)
    val result = mapping.unmap(mapping.map(Some("text")))
    assertEquals(result, Right(Some("text")))
  }

  test("the optional mapping works for none") {
    val mapping = optional(text)
    val result = mapping.unmap(mapping.map(None))
    assertEquals(result, Right(None))
  }

  test("the optional mapping is ok with missing value") {
    val mapping = optional(text)
    val result = mapping.unmap(Map())
    assertEquals(result, Right(None))
  }

  test("ignored will use the constant value") {
    val mapping = ignored(1)
    val result = mapping.unmap(Map("" -> 2))
    assertEquals(result, Right(1))
  }

  test("keep will just pass the value through") {
    val mapping = keep[Int]
    val mapped = mapping.map(1)
    assertEquals(mapped, Map("" -> 1))
    val result = mapping.unmap(mapped)
    assertEquals(result, Right(1))
  }

}
