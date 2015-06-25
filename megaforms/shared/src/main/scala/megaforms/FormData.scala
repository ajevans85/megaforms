package megaforms

import scala.collection.immutable.Seq


/**
 * Logic for changing the mapped data in a form in common scenarios.
 */
object FormData {

  /** suffix this key with an index to address a specific indexed field */
  def keyWithIndex(key: String, index: Int): String =
    if (key.isEmpty) s"[$index]"
    else s"$key.[$index]"

  /** suffix this key with a subkey to address a nested value */
  def keyWithChild(path: String, subkey: String): String = {
    if (path.isEmpty) subkey
    else if (subkey.isEmpty) path
    else s"$path.$subkey"
  }

  /** @return All the numeric indices for a seq mapping inside a data map */
  def indexesFor(path: String, data: Map[String, Any]): Seq[Int] = {
    val KeyPattern = ("^" + java.util.regex.Pattern.quote(path) + """\.\[(\d+)\].*$""").r
    data.toList.collect { case (KeyPattern(index), _) => index.toInt }.sorted.distinct
  }

  /**
   * Moves all indexed data under 'key' shift steps. Like so:
   * {{{
   *   val data = Map("root.[0].a" -> "a", "root.[0].b" -> "b")
   *   val result = shiftIndexed("root", 1, data)
   *   result == Map("root.[1].a" -> "a", "root.[1].b" -> "b")
   * }}}
   *
   * @param key Root key that contains indexed values
   * @param shift How many steps to shift
   */
  def shiftIndexed(key: String, shift: Int, data: Map[String, Any]): Map[String, Any] = {
    val existingIndices: Seq[Int] = indexesFor(key, data).reverse

    existingIndices.foldLeft(data) { case (map, index) =>
      reindex(key, index, index + shift, map)
    }
  }

  /** add a prefix to every entry in a form data map */
  def prefixAll(prefix: String, data: Map[String, Any]): Map[String, Any] =
    data.map { case (key, value) => keyWithChild(prefix, key) -> value }

  /** remove each sub field from a form data map, note that this might create a
    * hole in an indexed seq, will also reindex the other existing sequence items */
  def remove(key: String, index: Int, data: Map[String, Any]): Map[String, Any] = {
    val indexedKey = keyWithIndex(key, index)
    val removed = data.filter { case (key, value) => !key.startsWith(indexedKey) }
    reindex(key, removed)
  }

  /** Make sure there are no holes in the given sequence path */
  def reindex(path: String, data: Map[String, Any]): Map[String, Any] = {
    val indexes = indexesFor(path, data)
    indexes.zipWithIndex.foldLeft(data) { case (data, (oldIndex, actualIndex)) =>
      reindex(path, oldIndex, actualIndex, data)
    }
  }

  /** Move all values under an indexed key to another index */
  def reindex(key: String, oldIndex: Int, newIndex: Int, data: Map[String, Any]): Map[String, Any] = {
    val oldPrefix = keyWithIndex(key, oldIndex)
    val newPrefix = keyWithIndex(key, newIndex)
    data.map {
      case (key, value) if key.startsWith(oldPrefix) => (newPrefix + key.drop(oldPrefix.length)) -> value
      case tuple => tuple
    }
  }

}
