package megaforms.validation

/**
 * Predefined constraints for a lot of generic use cases (does not know about types outside of
 * the scala std library)
 */
object Constraints {

  /** anything goes */
  def noValidation[T]: Constraint[T] = Constraint(n => None)

  /** fail if the text is empty (has zero length or only contains whitespace) */
  val nonEmptyText: Constraint[String] = Constraint(str =>
    if (str.trim.isEmpty) Some("Required")
    else None
  )

  def maxLength(chars: Int): Constraint[String] = Constraint { text =>
    if (text.length > chars) Some(s"Text cannot be longer than $chars characters")
    else None
  }

  /**
   * @param expr A regular expression that a text value must match
   * @param onErr The error message to show if the text does not match, input parameter is the value
   *              that didn't match the regex
   */
  def regex(expr: String, onErr: String => String): Constraint[String] = Constraint(str =>
    if (!str.matches(expr)) Some(expr)
    else None
  )

  /**
   * @param expr A regular expression that a text value must match
   * @param onErr The error message to show if the text does not match
   */
  def regex(expr: String, onErr: => String): Constraint[String] = regex(expr, _ => onErr)

  /** Fails if the numeric value is higher than ```maxValue``` */
  def max[N : Numeric](maxValue: N): Constraint[N] = {
    val numeric = implicitly[Numeric[N]]
    Constraint(value =>
      if (numeric.gt(value, maxValue)) Some(s"Max value $maxValue")
      else None
    )
  }

  /** Fails if the numeric value is lower than ```minValue``` */
  def min[N : Numeric](minValue: N): Constraint[N] = {
    val numeric = implicitly[Numeric[N]]
    Constraint(value =>
      if (numeric.lt(value, minValue)) Some(s"Min value $minValue")
      else None
    )
  }

  /** Fails if the numeric value is not between or equal to the ```minimum``` and ```maximum``` values */
  def between[N : Numeric](minimum: N, maximum: N): Constraint[N] =
    min[N](minimum).and(max[N](maximum))

  /** Fails if the numeric value is negative */
  def positive[N : Numeric]: Constraint[N] = {
    val numeric = implicitly[Numeric[N]]
    Constraint[N]((value: N)=>
      if (numeric.signum(value) == -1) Some("The value may not be negative")
      else None
    )
  }

}