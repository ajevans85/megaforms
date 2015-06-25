package megaforms.mapping

import megaforms.validation.Constraint
import megaforms.FormError

import scala.collection.immutable.Seq

/**
 * Maps one concrete field to and from a value T
 */
private[megaforms] case class FieldMapping[T](
    path: String,
    format: Format[T],
    constraints: Seq[Constraint[T]] = Seq.empty
  ) extends Mapping[T] {

  def validate(t: T): Seq[FormError] =
    constraints.flatMap(_.validate(t)).map(error => FormError(path, error))

  def unmap(data: Map[String, Any]): Either[Seq[FormError], T] =
    format.parse(data(path)).fold(
      error => Left(Seq(FormError(path, error))),
      { t =>
        val validationErrors = validate(t)
        if (validationErrors.isEmpty) Right(t)
        else Left(validationErrors)
      }
    )


  override def map(t: T): Map[String, Any] =
    Map(path -> format.format(t))

  override def verifying(additional: Constraint[T]*): Mapping[T] =
    copy(constraints = constraints ++ additional)

  override def withPrefix(prefix: String): Mapping[T] = copy(path = addPrefix(prefix))

}