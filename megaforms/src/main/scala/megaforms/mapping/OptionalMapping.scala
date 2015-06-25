package megaforms.mapping

import megaforms.FormError
import megaforms.validation.Constraint

import scala.collection.immutable.Seq

/**
 * Allows for optional mappings, turning a ```Mapping[T]``` into a ```Mapping[Option[T]]```
 * where the value might be missing. Will not introduce
 * a new level in the path, just handle that the value might be missing when mapping and unmapping
 */
private[megaforms] case class OptionalMapping[T](wrapped: Mapping[T], constraints: Seq[Constraint[Option[T]]] = Seq.empty) extends Mapping[Option[T]] {

  override def path = wrapped.path

  override def map(t: Option[T]): Map[String, Any] =
    t.fold[Map[String, Any]](
      Map.empty
    )(value =>
      wrapped.map(value)
    )

  override def unmap(data: Map[String, Any]): Either[Seq[FormError], Option[T]] =
    if (data.contains(path)) wrapped.unmap(data).right.map(Some(_))
    else Right(None)

  override def validate(t: Option[T]): Seq[FormError] =
    constraints.flatMap(_.validate(t).map(FormError(path, _))) ++
      t.fold[Seq[FormError]](
        Seq.empty
      )(value =>
        wrapped.validate(value)
      )

  override def verifying(additionalConstraints: Constraint[Option[T]]*): Mapping[Option[T]] =
    copy(constraints = constraints ++ additionalConstraints)

  override def withPrefix(prefix: String): Mapping[Option[T]] = copy(wrapped = wrapped.withPrefix(prefix))
}
