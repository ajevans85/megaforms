package megaforms.validation

/**
 * A validation rule that can say if a value of T is good or what is wrong with it
 */
trait Constraint[T] {
  /** @return a description of what is wrong or None if the value is ok */
  def validate(t: T): Option[String]

  /** combine this validation rule with another, both must be valid for this constraint
    * to be satisfied, basically the same as validating each in a list of validations,
    * but will stop at the first validation error rather than to evaluate all of them */
  def and(other: Constraint[T]): Constraint[T] = new AndConstraint[T](this, other)
  /** combine this validation rule with another, if one of them is valid that is enough
    * for the combined constraint to be valid */
  def or(other: Constraint[T]): Constraint[T] = new OrConstraint[T](this, other)
}

/** both validations must pass */
private[megaforms] case class AndConstraint[T](a: Constraint[T], b: Constraint[T]) extends Constraint[T] {
  override def validate(t: T): Option[String] = a.validate(t).orElse(b.validate(t))
}

/** if either validation passes, this is ok */
private[megaforms] case class OrConstraint[T](a: Constraint[T], b: Constraint[T]) extends Constraint[T] {
  override def validate(t: T): Option[String] = {
    val aResult = a.validate(t)
    val bResult = b.validate(t)
    if (aResult.isEmpty || bResult.isEmpty) None
    else aResult.orElse(bResult)
  }
}

object Constraint {

  /** shortcut to easily create a constraint using a closure
    * @param f A function returning ```Some(error)``` if a ```T``` is invalid or ```None``` if a ```T``` is ok
    */
  def apply[T](f: T => Option[String]): Constraint[T] = new Constraint[T] {
    override def validate(t: T): Option[String] = f(t)
  }

}