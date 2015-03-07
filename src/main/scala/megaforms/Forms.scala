package megaforms

import mapping.{Mapping => ActualMapping, _}

import scala.collection.immutable.Seq

/**
 * Single entry point of the forms api, provides all imports you need.
 */
object Forms {

  // quick imports
  type Mapping[T] = ActualMapping[T]
  type Form[T] = megaforms.Form[T]
  type Constraint[T] = megaforms.validation.Constraint[T]
  val Constraint = validation.Constraint

  /** common re-usable constraints.
    * @see [[megaforms.validation.Constraints]]
    */
  val Constraints = validation.Constraints

  // composed mapping shortcuts

  /** a repeated set of T:s mapping each T with the given mapping
    * Note that it currently does not work using a seq as the root mapping, there must
    * be a parent mapping.
    */
  def seq[T](itemMapping: Mapping[T]): Mapping[Seq[T]] = SeqMapping("", itemMapping)

  /** a mapping to a concrete one field type */
  def mapping[T, F1](f1: (String, Mapping[F1]))(apply: F1 => T)(unapply: T => Option[F1]): Mapping[T] =
    ObjectMapping1[T, F1](apply, unapply, f1)

  /** a mapping to a concrete two field type */
  def mapping[T, F1, F2](f1: (String, Mapping[F1]), f2: (String, Mapping[F2]))(apply: (F1, F2) => T)(unapply: T => Option[(F1, F2)]): Mapping[T] =
    ObjectMapping2[T, F1, F2](apply, unapply, f1, f2)

  /** a mapping to a concrete three field type */
  def mapping[T, F1, F2, F3](f1: (String, Mapping[F1]), f2: (String, Mapping[F2]), f3: (String, Mapping[F3]))(apply: (F1, F2, F3) => T)(unapply: T => Option[(F1, F2, F3)]): Mapping[T] =
    ObjectMapping3[T, F1, F2, F3](apply, unapply, f1, f2, f3)

  /** a mapping to a concrete four field type */
  def mapping[T, F1, F2, F3, F4](f1: (String, Mapping[F1]), f2: (String, Mapping[F2]), f3: (String, Mapping[F3]), f4: (String, Mapping[F4]))(apply: (F1, F2, F3, F4) => T)(unapply: T => Option[(F1, F2, F3, F4)]): Mapping[T] =
    ObjectMapping4[T, F1, F2, F3, F4](apply, unapply, f1, f2, f3, f4)

  /** a mapping to a concrete five field type */
  def mapping[T, F1, F2, F3, F4, F5](f1: (String, Mapping[F1]), f2: (String, Mapping[F2]), f3: (String, Mapping[F3]), f4: (String, Mapping[F4]), f5: (String, Mapping[F5]))(apply: (F1, F2, F3, F4, F5) => T)(unapply: T => Option[(F1, F2, F3, F4, F5)]): Mapping[T] =
    ObjectMapping5[T, F1, F2, F3, F4, F5](apply, unapply, f1, f2, f3, f4, f5)

  /** a mapping to a concrete six field type */
  def mapping[T, F1, F2, F3, F4, F5, F6](f1: (String, Mapping[F1]), f2: (String, Mapping[F2]), f3: (String, Mapping[F3]), f4: (String, Mapping[F4]), f5: (String, Mapping[F5]), f6: (String, Mapping[F6]))(apply: (F1, F2, F3, F4, F5, F6) => T)(unapply: T => Option[(F1, F2, F3, F4, F5, F6)]): Mapping[T] =
    ObjectMapping6[T, F1, F2, F3, F4, F5, F6](apply, unapply, f1, f2, f3, f4, f5, f6)

  /** a mapping to a concrete seven field type */
  def mapping[T, F1, F2, F3, F4, F5, F6, F7](f1: (String, Mapping[F1]), f2: (String, Mapping[F2]), f3: (String, Mapping[F3]), f4: (String, Mapping[F4]), f5: (String, Mapping[F5]), f6: (String, Mapping[F6]), f7: (String, Mapping[F7]))(apply: (F1, F2, F3, F4, F5, F6, F7) => T)(unapply: T => Option[(F1, F2, F3, F4, F5, F6, F7)]): Mapping[T] =
    ObjectMapping7[T, F1, F2, F3, F4, F5, F6, F7](apply, unapply, f1, f2, f3, f4, f5, f6, f7)



  def tuple[A, B](f1: (String, Mapping[A]), f2: (String, Mapping[B])): Mapping[(A, B)] =
    ObjectMapping2[(A, B), A, B](Tuple2.apply, Tuple2.unapply, f1, f2)

  def tuple[A, B, C](f1: (String, Mapping[A]), f2: (String, Mapping[B]), f3: (String, Mapping[C])): Mapping[(A, B, C)] =
    ObjectMapping3[(A, B, C), A, B, C](Tuple3.apply, Tuple3.unapply, f1, f2, f3)


  def optional[T](mapping: Mapping[T]): Mapping[Option[T]] = new OptionalMapping[T](mapping)

  // field mapping shortcuts
  val text: Mapping[String] = FieldMapping("", format = Formats.Text)
  val integer: Mapping[Int] = FieldMapping("", format = Formats.Integer)
  def decimal(decimals: Int): Mapping[BigDecimal] = FieldMapping("", format = Formats.Decimal(decimals))

  /** true / false or lack of value, which is treated as false */
  val boolean: Mapping[Boolean] =
    optional[Boolean](FieldMapping("", format = Formats.Boolean))
      .transform(maybe => maybe.getOrElse(false), bool => Some(bool))

  // special mappings

  /** provide a constant value that will not be editable, when the form is valid this mapping
    * will always contain 'value'
    */
  def ignored[T](value: T): Mapping[T] = FieldMapping("", format = Formats.ignored(value))
  /** provide a mapping that does not modify an object */
  def keep[T]: Mapping[T] = FieldMapping("", format = Formats.keep[T])




}
