package megaforms.mapping

import megaforms.validation.Constraint
import megaforms.{FormData, FormError}

import scala.collection.immutable.Seq


/**
 * Maps a sequence of ```T```s, used for repeated elements in a mapping. The mapped representation will be keyed like this:
 * {{{
 *   Map(
 *     "root.[0].propA" -> value1
 *     "root.[0].propB" -> value
 *   )
 * }}}
 *
 * @param path
 * @param wrapped the mapping of each element
 * @param constraints Constraints for the entire sequence (for example how many there can be, if there can be none)
 */
private[megaforms] case class SeqMapping[T](
    path: String,
    wrapped: Mapping[T],
    constraints: Seq[Constraint[Seq[T]]] = Seq.empty
  ) extends Mapping[Seq[T]] {

  override def validate(ts: Seq[T]): Seq[FormError] =
    ts.flatMap(t => wrapped.validate(t))

  override def verifying(additional: Constraint[Seq[T]]*): Mapping[Seq[T]] =
    copy(constraints = constraints ++ additional)

  // we just nest a seq of maps inside of the map under our key
  override def map(ts: Seq[T]): Map[String, Any] =
    ts.zipWithIndex
      .flatMap { case (t, index) => wrapped.withPrefix(FormData.keyWithIndex(path, index)).map(t) }
      .toMap


  override def unmap(data: Map[String, Any]): Either[Seq[FormError], Seq[T]] = {
    val indexes = FormData.indexesFor(path, data)

    val unmapped = indexes.map(index => wrapped.withPrefix(FormData.keyWithIndex(path, index)).unmap(data))

    // if there is any error, all of it is an error, if all ok, the sequence is ok
    val errors = unmapped.collect { case Left(e) => e }
    if (errors.isEmpty) Right(unmapped.collect { case Right(value) => value })
    else Left(errors.flatten)
  }

  override def withPrefix(prefix: String): Mapping[Seq[T]] = copy(path = addPrefix(prefix))

}
