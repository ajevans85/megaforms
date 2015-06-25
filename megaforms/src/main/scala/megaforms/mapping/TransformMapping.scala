package megaforms.mapping

import megaforms.FormError
import megaforms.validation.Constraint

import scala.collection.immutable.Seq

/**
 * Allows for performing a bidirectional transformation of a ```Mapping[T]``` into a ```Mapping[B]``` using
 * two functions ```T => B``` and ```B => T```
 */
private[megaforms] case class TransformMapping[T, B](
    wrapped: Mapping[T],
    transform: T => B,
    untransform: B => T,
    constraints: Seq[Constraint[B]] = Seq.empty) extends Mapping[B] {

  override def path: String = wrapped.path

  override def verifying(additionalConstraints: Constraint[B]*): Mapping[B] =
    copy(constraints = constraints ++ additionalConstraints)

  override def withPrefix(key: String): Mapping[B] =
    copy(wrapped = wrapped.withPrefix(key))

  /** apply the validation constraints of this mapping and return the result */
  override def validate(b: B): Seq[FormError] =
    constraints.flatMap(_.validate(b).map(FormError(path, _))) ++
      wrapped.validate(untransform(b))

  /** transform the type unsafe map of key -> any kind of value to a T if valid, or a sequence
    * of errors describing what is wrong with it if it isn't valid/impossible to create a T out of
    */
  override def unmap(data: Map[String, Any]): Either[Seq[FormError], B] =
    wrapped.unmap(data) match {
      case Right(t) =>
        val b = transform(t)
        val errors = validate(b)
        if (errors.isEmpty) Right(b)
        else Left(errors)

      case Left(errors) => Left(errors)
    }

  /** transform a t to the a type unsafe map of key -> value */
  override def map(b: B): Map[String, Any] =
    wrapped.map(untransform(b))
}
