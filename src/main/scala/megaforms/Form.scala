package megaforms

import scala.collection.immutable.Seq
import scala.language.existentials
import mapping.Mapping

/**
 * A form represents the current state of a form, it knows of the mapping - how to map a tree of
 * field values into the value T and how to validate. Decoupled from react.
 *
 * @param data The raw unbound/unmapped data, a flat map, so each concrete field will have a unique key here, even
 *             if it is a nested field. Nested field will have their keys separated by a dot,
 *             Sequences will have a index indicator [n] as part of their key.
 *             (Using a flat map is to avoid a single field update having to recreate a big part of an object graph)
 * @param value If possible to parse the raw data into a T this will be that T
 * @param errors If there was error parsing a T then this is those
 * @tparam T The type of object that the form represents
 */
final case class Form[T](mapping: Mapping[T], data: Map[String, Any] = Map.empty, value: Option[T] = None, errors: Seq[FormError] = Seq.empty) {

  /** errors for the entire form, not individual fields */
  def globalErrors: Seq[FormError] = errors.filter(_.path == "")

  /** get the field model for a given key path inside the form,
    * used for integrating with the actual react form fieds.
    */
  def apply(path: String): Field = {
    new Field(
      this,
      path
    )
  }

  def errors(path: String): Seq[String] = errors.collect { case FormError(errKey, msg) if errKey == path => msg }

  /**
   * Fill the form with t and validate it (if t does not adhere to the constraints/validation
   * the form will not have t as value, but the field values from reading it)
   */
  def fill(t: T): Form[T] = {
    val (bound, errors) = mapping.mapAndValidate(t)
    copy(
      data = bound,
      value = if (errors.isEmpty) Some(t) else None,
      errors = errors)
  }

  /** create a new form with the given field updated, possibly unmapped into a T and validation errors */
  def update(path: String, value: Any): Form[T] =
    update(data + (path -> value))

  /** create a new form with the given state, possibly unmapped T and validation errors */
  def update(newData: Map[String, Any]): Form[T] = {
    val (newErrors, newValue) =
      mapping
        .unmap(newData)
        .fold[(Seq[FormError], Option[T])](
          errors => (errors, None),
          newValue => (Seq.empty, Some(newValue))
        )

    copy(
      data = newData,
      value = newValue,
      errors = newErrors
    )
  }

}

