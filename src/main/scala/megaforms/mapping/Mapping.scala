package megaforms.mapping

import megaforms.{FormData, FormError}
import megaforms.validation.Constraint

import scala.collection.immutable.Seq


/**
 * A mapping is a bidirectional mapping between a type safe field/value and type unsafe
 * ```Map[String, Any]``` that is suitable to use when integrating with actual html form fields.
 */
trait Mapping[T] {

  /** A direct id/key for a field or a dot separated path in the case of nested mappings. */
  private[megaforms] def path: String

  /** constraints limiting what values are ok for this field */
  private[megaforms] def constraints: Seq[Constraint[T]]

  /** apply the validation constraints of this mapping and return the result */
  private[megaforms] def validate(t: T): Seq[FormError]

  /** transform the type unsafe map of key -> any kind of value to a ```T``` if valid, or a sequence
    * of errors describing what is wrong with it if it isn't valid/impossible to create a ```T``` out of
    */
  private[megaforms] def unmap(data: Map[String, Any]): Either[Seq[FormError], T]

  /** transform a t to the a type unsafe map of key -> value */
  private[megaforms] def map(t: T): Map[String, Any]

  /** do map and validate in one step (useful for mapping an existing value that might contain
    * invalid data from the beginning)
    */
  private[megaforms] def mapAndValidate(t: T): (Map[String, Any], Seq[FormError]) =
    (map(t), validate(t))

  /** the same mapping but with the path prefixed with key, so the key "prop" would become "prefix.prop"
    * if current key is empty "" becomes "prefix" */
  private[megaforms] def withPrefix(key: String): Mapping[T]

  /** the same mapping but with the given constraint appended to the list of validation constraints */
  def verifying(additionalConstraints: Constraint[T]*): Mapping[T]

  /** transform this mapping into a mapping of another type (```B```) by providing functions
    * that can create a ```B``` from a ```T``` and vice versa.
    */
  def transform[B](map: T => B, unmap: B => T): Mapping[B] = TransformMapping[T, B](this, map, unmap)

  /** prefix the current key with the given key, handles inserting path separator etc. */
  protected def addPrefix(prefix: String): String =
    FormData.keyWithChild(prefix, path)

}


