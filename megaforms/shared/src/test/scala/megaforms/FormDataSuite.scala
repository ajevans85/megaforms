package megaforms

import minitest.SimpleTestSuite

object FormDataSuite extends SimpleTestSuite {

  test("The SeqMapping indexes thingie can find all indexes") {
    val input = Map(
      "root.[0].value" -> 0,
      "root.[1].value" -> 0
    )

    val result = FormData.indexesFor("root", input)

    assertEquals(result, Seq(0, 1))
  }

  test("The index shifting can move indexes one step up") {
    val data = Map("root.[0].a" -> "a1", "root.[0].b" -> "b1", "root.[1].a" -> "a2")
    val result = FormData.shiftIndexed("root", 1, data)

    assertEquals(result, Map("root.[1].a" -> "a1", "root.[1].b" -> "b1", "root.[2].a" -> "a2"))
  }

  test("it can prefix all keys with a given path") {
    val data = Map("root.[0].a" -> "a1", "root.[0].b" -> "b1", "root.[1].a" -> "a2")
    val result = FormData.prefixAll("prefix", data)

    assertEquals(result,  Map("prefix.root.[0].a" -> "a1", "prefix.root.[0].b" -> "b1", "prefix.root.[1].a" -> "a2"))
  }

  test("The indexed key path generator appends index with []") {
    assertEquals(FormData.keyWithIndex("root", 5), "root.[5]")
  }

  test("The indexed key path generator uses just [n] if key is empty") {
    assertEquals(FormData.keyWithIndex("", 5), "[5]")
  }

  test("remove all removes an indexed position (and re-index the sequence to not leave any holes)") {
    val result = FormData.remove("a", 0, Map("a.[0].b1" -> 0, "a.[0].b2" -> 1, "a.[1].b1" -> 2))
    assertEquals(result, Map("a.[0].b1" -> 2))
  }

  test("re-index a indexed key") {
    val result = FormData.reindex("a", 1, 2, Map("a.[0].b1" -> 0, "a.[0].b2" -> 1, "a.[1].b1" -> 0))
    assertEquals(result, Map("a.[0].b1" -> 0, "a.[0].b2" -> 1, "a.[2].b1" -> 0))
  }

  test("remove holes in a sequence") {
    val result = FormData.reindex("a", Map("a.[2].b1" -> 0, "a.[4].b1" -> 1, "a.[6].b1" -> 2))
    assertEquals(result, Map("a.[0].b1" -> 0, "a.[1].b1" -> 1, "a.[2].b1" -> 2))
  }

}
