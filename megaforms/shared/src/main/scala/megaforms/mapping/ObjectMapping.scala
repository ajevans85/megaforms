package megaforms.mapping

import megaforms.validation.Constraint
import megaforms.FormError

import scala.collection.immutable.Seq



/**
 * An object mapping maps from one concrete type (T) to a sequence of submappings, one per field of the top type
 * by using an apply F1 ... Fn => T and an unapply T => Option[F1 ... Fn] to going inbetween field values
 * and the concrete type.
 *
 * The mapped value will be sub-keyed under the mapping key itself, so
 * {{{
 * case class MyClass(name: String)
 * ObjectMapping1[MyClass, String](path = "root", f1 = name" -> text, apply = MyClass.apply, unapply = MyClass.unapply)
 * }}}
 * would map {{{MyClass("nils")}}} to {{{Map("root.name" -> "nils")}}}
 *
 * (This trait contains common logic for the ObjectMappingN-classes rather than a common api, look below
 * for implementations for a class with N fields)
 */
private[megaforms] trait ObjectMapping[T] {

  def path: String
  def constraints: Seq[Constraint[T]]

  protected def validateSelf(t: T): Seq[FormError] =
    constraints.flatMap(_.validate(t)).map(error => FormError(path, error))

  protected def unmapAndValidate(t: T): Either[Seq[FormError], T] = {
    val errors = validateSelf(t)
    if (errors.isEmpty) Right(t)
    else Left(errors)
  }

  /** combine into either all anys or all errors if there is any error */
  protected def merge(results: Either[Seq[FormError], Any]*) = {
    def merge2(a: Either[Seq[FormError], Seq[Any]], b: Either[Seq[FormError], Any]): Either[Seq[FormError], Seq[Any]] =
      (a, b) match {
        case (Left(aErrors), Left(bErrors)) => Left(aErrors ++ bErrors)
        case (Left(aErrors), Right(_)) => Left(aErrors)
        case (Right(_), Left(bErrors)) => Left(bErrors)
        case (Right(as), Right(b)) => Right(as :+ b)
      }

    results.foldLeft[Either[Seq[FormError], Seq[Any]]](Right(Seq.empty[Any]))(merge2)
  }

}

// these could maybe be created with either a macro or some typelevel stuff, but for now
// handmade, to get that special taste, mmm mmm mm

/**
 * Mapping for a type with one field
 * see [[ObjectMapping]]
 */
private[megaforms] case class ObjectMapping1[T, F1](
    apply: F1 => T,
    unapply: T => Option[F1],
    f1: (String, Mapping[F1]),
    path: String = "",
    constraints: Seq[Constraint[T]] = Seq.empty
  ) extends Mapping[T] with ObjectMapping[T] {

  val field1 = f1._2.withPrefix(f1._1).withPrefix(path)

  override def validate(t: T): Seq[FormError] = 
     validateSelf(t) ++ unapply(t).fold[Seq[FormError]](Seq.empty)(field1.validate)

  override def map(t: T): Map[String, Any] =
    unapply(t).fold[Map[String, Any]](
      Map.empty
    )(val1 =>
      Map(f1._1 -> val1)
    )

  override def unmap(data: Map[String, Any]): Either[Seq[FormError], T] =
    field1.unmap(data).right.flatMap(v1 => unmapAndValidate(apply(v1)))

  override def withPrefix(prefix: String): Mapping[T] = copy(path = addPrefix(prefix))

  override def verifying(additional: Constraint[T]*): Mapping[T] =
    copy(constraints = constraints ++ additional)
}

/**
 * Mapping for a type with two fields (of type F1 and F2)
 * see [[ObjectMapping]]
 */
private[megaforms] case class ObjectMapping2[T, F1, F2](
    apply: (F1, F2) => T,
    unapply: T => Option[(F1, F2)],
    f1: (String, Mapping[F1]),
    f2: (String, Mapping[F2]),
    path: String = "",
    constraints: Seq[Constraint[T]] = Seq.empty
  ) extends Mapping[T] with ObjectMapping[T] {

  val field1 = f1._2.withPrefix(f1._1).withPrefix(path)
  val field2 = f2._2.withPrefix(f2._1).withPrefix(path)

  override def validate(t: T): Seq[FormError] =
    validateSelf(t) ++
      unapply(t).fold[Seq[FormError]](Seq.empty) {
        case (val1, val2) => field1.validate(val1) ++ field2.validate(val2)
      }

  override def map(t: T): Map[String, Any] =
    unapply(t).fold[Map[String, Any]](
      Map.empty
    ) {
      case (val1, val2) => field1.map(val1) ++ field2.map(val2)
    }

  override def unmap(data: Map[String, Any]): Either[Seq[FormError], T] =
    merge(field1.unmap(data), field2.unmap(data)).right.flatMap(fs =>
      unmapAndValidate(apply(
        fs(0).asInstanceOf[F1],
        fs(1).asInstanceOf[F2]
      ))
    )

  override def withPrefix(prefix: String): Mapping[T] = copy(path = addPrefix(prefix))

  override def verifying(additional: Constraint[T]*): Mapping[T] =
    copy(constraints = constraints ++ additional)
}

/**
 * Mapping for a type with three fields (of type F1, F2 and F3)
 * see [[ObjectMapping]]
 */
private[megaforms] case class ObjectMapping3[T, F1, F2, F3](
    apply: (F1, F2, F3) => T,
    unapply: T => Option[(F1, F2, F3)],
    f1: (String, Mapping[F1]),
    f2: (String, Mapping[F2]),
    f3: (String, Mapping[F3]),
    path: String = "",
    constraints: Seq[Constraint[T]] = Seq.empty
  ) extends Mapping[T] with ObjectMapping[T] {

  val field1 = f1._2.withPrefix(f1._1).withPrefix(path)
  val field2 = f2._2.withPrefix(f2._1).withPrefix(path)
  val field3 = f3._2.withPrefix(f3._1).withPrefix(path)

  override def validate(t: T): Seq[FormError] =
    validateSelf(t) ++
      unapply(t).fold[Seq[FormError]](Seq.empty) {
        case (val1, val2, val3) => field1.validate(val1) ++ field2.validate(val2) ++ field3.validate(val3)
      }

  override def map(t: T): Map[String, Any] =
    unapply(t).fold[Map[String, Any]](
      Map.empty
    ) {
      case (val1, val2, val3) => field1.map(val1) ++ field2.map(val2) ++ field3.map(val3)
    }

  override def unmap(data: Map[String, Any]): Either[Seq[FormError], T] =
    merge(field1.unmap(data), field2.unmap(data), field3.unmap(data)).right.flatMap(fs =>
      unmapAndValidate(apply(
        fs(0).asInstanceOf[F1],
        fs(1).asInstanceOf[F2],
        fs(2).asInstanceOf[F3])
      )
    )

  override def withPrefix(prefix: String): Mapping[T] = copy(path = addPrefix(prefix))

  override def verifying(additional: Constraint[T]*): Mapping[T] =
    copy(constraints = constraints ++ additional)

}

/**
 * Mapping for a type with four fields (of type F1, F2, F3 and F4)
 * see [[ObjectMapping]]
 */
private[megaforms] case class ObjectMapping4[T, F1, F2, F3, F4](
    apply: (F1, F2, F3, F4) => T,
    unapply: T => Option[(F1, F2, F3, F4)],
    f1: (String, Mapping[F1]),
    f2: (String, Mapping[F2]),
    f3: (String, Mapping[F3]),
    f4: (String, Mapping[F4]),
    path: String = "",
    constraints: Seq[Constraint[T]] = Seq.empty
  ) extends Mapping[T] with ObjectMapping[T] {

  val field1 = f1._2.withPrefix(f1._1).withPrefix(path)
  val field2 = f2._2.withPrefix(f2._1).withPrefix(path)
  val field3 = f3._2.withPrefix(f3._1).withPrefix(path)
  val field4 = f4._2.withPrefix(f4._1).withPrefix(path)

  override def validate(t: T): Seq[FormError] =
    validateSelf(t) ++
      unapply(t).fold[Seq[FormError]](Seq.empty) {
        case (val1, val2, val3, val4) =>
          field1.validate(val1) ++ field2.validate(val2) ++ field3.validate(val3) ++ field4.validate(val4)
      }

  override def map(t: T): Map[String, Any] =
    unapply(t).fold[Map[String, Any]](
      Map.empty
    ) {
      case (val1, val2, val3, val4) => field1.map(val1) ++ field2.map(val2) ++ field3.map(val3) ++ field4.map(val4)
    }

  override def unmap(data: Map[String, Any]): Either[Seq[FormError], T] =
    merge(field1.unmap(data), field2.unmap(data), field3.unmap(data), field4.unmap(data)).right.flatMap(fs =>
      unmapAndValidate(apply(
        fs(0).asInstanceOf[F1],
        fs(1).asInstanceOf[F2],
        fs(2).asInstanceOf[F3],
        fs(3).asInstanceOf[F4]
      ))
    )

  override def withPrefix(prefix: String): Mapping[T] = copy(path = addPrefix(prefix))

  override def verifying(additional: Constraint[T]*): Mapping[T] =
    copy(constraints = constraints ++ additional)

}

/**
 * Mapping for a type with five fields (of type F1, F2, F3, F4 and F5)
 * see [[ObjectMapping]]
 */
private[megaforms] case class ObjectMapping5[T, F1, F2, F3, F4, F5](
    apply: (F1, F2, F3, F4, F5) => T,
    unapply: T => Option[(F1, F2, F3, F4, F5)],
    f1: (String, Mapping[F1]),
    f2: (String, Mapping[F2]),
    f3: (String, Mapping[F3]),
    f4: (String, Mapping[F4]),
    f5: (String, Mapping[F5]),
    path: String = "",
    constraints: Seq[Constraint[T]] = Seq.empty
  ) extends Mapping[T] with ObjectMapping[T] {

  val field1 = f1._2.withPrefix(f1._1).withPrefix(path)
  val field2 = f2._2.withPrefix(f2._1).withPrefix(path)
  val field3 = f3._2.withPrefix(f3._1).withPrefix(path)
  val field4 = f4._2.withPrefix(f4._1).withPrefix(path)
  val field5 = f5._2.withPrefix(f5._1).withPrefix(path)

  override def validate(t: T): Seq[FormError] =
    validateSelf(t) ++
      unapply(t).fold[Seq[FormError]](Seq.empty) {
        case (val1, val2, val3, val4, val5) =>
          field1.validate(val1) ++ field2.validate(val2) ++ field3.validate(val3) ++ field4.validate(val4) ++ field5.validate(val5)
      }

  override def map(t: T): Map[String, Any] =
    unapply(t).fold[Map[String, Any]](
      Map.empty
    ) {
      case (val1, val2, val3, val4, val5) => field1.map(val1) ++ field2.map(val2) ++ field3.map(val3) ++ field4.map(val4) ++ field5.map(val5)
    }

  override def unmap(data: Map[String, Any]): Either[Seq[FormError], T] =
    merge(field1.unmap(data), field2.unmap(data), field3.unmap(data), field4.unmap(data), field5.unmap(data)).right.flatMap(fs =>
      unmapAndValidate(apply(
        fs(0).asInstanceOf[F1],
        fs(1).asInstanceOf[F2],
        fs(2).asInstanceOf[F3],
        fs(3).asInstanceOf[F4],
        fs(4).asInstanceOf[F5]
      ))
    )

  override def withPrefix(prefix: String): Mapping[T] = copy(path = addPrefix(prefix))

  override def verifying(additional: Constraint[T]*): Mapping[T] =
    copy(constraints = constraints ++ additional)

}

/**
 * Mapping for a type with six fields (of type F1, F2, F3, F4, F5 and F6)
 * see [[ObjectMapping]]
 */
private[megaforms] case class ObjectMapping6[T, F1, F2, F3, F4, F5, F6](
    apply: (F1, F2, F3, F4, F5, F6) => T,
    unapply: T => Option[(F1, F2, F3, F4, F5, F6)],
    f1: (String, Mapping[F1]),
    f2: (String, Mapping[F2]),
    f3: (String, Mapping[F3]),
    f4: (String, Mapping[F4]),
    f5: (String, Mapping[F5]),
    f6: (String, Mapping[F6]),
    path: String = "",
    constraints: Seq[Constraint[T]] = Seq.empty
  ) extends Mapping[T] with ObjectMapping[T] {

  val field1 = f1._2.withPrefix(f1._1).withPrefix(path)
  val field2 = f2._2.withPrefix(f2._1).withPrefix(path)
  val field3 = f3._2.withPrefix(f3._1).withPrefix(path)
  val field4 = f4._2.withPrefix(f4._1).withPrefix(path)
  val field5 = f5._2.withPrefix(f5._1).withPrefix(path)
  val field6 = f6._2.withPrefix(f6._1).withPrefix(path)

  override def validate(t: T): Seq[FormError] =
    validateSelf(t) ++
      unapply(t).fold[Seq[FormError]](Seq.empty) {
        case (val1, val2, val3, val4, val5, val6) =>
          field1.validate(val1) ++ field2.validate(val2) ++ field3.validate(val3) ++ field4.validate(val4) ++ field5.validate(val5) ++ field6.validate(val6)
      }

  override def map(t: T): Map[String, Any] =
    unapply(t).fold[Map[String, Any]](
      Map.empty
    ) {
      case (val1, val2, val3, val4, val5, val6) => field1.map(val1) ++ field2.map(val2) ++ field3.map(val3) ++ field4.map(val4) ++ field5.map(val5) ++ field6.map(val6)
    }

  override def unmap(data: Map[String, Any]): Either[Seq[FormError], T] =
    merge(field1.unmap(data), field2.unmap(data), field3.unmap(data), field4.unmap(data), field5.unmap(data), field6.unmap(data)).right.flatMap(fs =>
      unmapAndValidate(apply(
        fs(0).asInstanceOf[F1],
        fs(1).asInstanceOf[F2],
        fs(2).asInstanceOf[F3],
        fs(3).asInstanceOf[F4],
        fs(4).asInstanceOf[F5],
        fs(5).asInstanceOf[F6]
      ))
    )

  override def withPrefix(prefix: String): Mapping[T] = copy(path = addPrefix(prefix))

  override def verifying(additional: Constraint[T]*): Mapping[T] =
    copy(constraints = constraints ++ additional)

}

/**
 * Mapping for a type with seven fields (of type F1, F2, F3, F4, F5, F6 and F7)
 * see [[ObjectMapping]]
 */
private[megaforms] case class ObjectMapping7[T, F1, F2, F3, F4, F5, F6, F7](
    apply: (F1, F2, F3, F4, F5, F6, F7) => T,
    unapply: T => Option[(F1, F2, F3, F4, F5, F6, F7)],
    f1: (String, Mapping[F1]),
    f2: (String, Mapping[F2]),
    f3: (String, Mapping[F3]),
    f4: (String, Mapping[F4]),
    f5: (String, Mapping[F5]),
    f6: (String, Mapping[F6]),
    f7: (String, Mapping[F7]),
    path: String = "",
    constraints: Seq[Constraint[T]] = Seq.empty
  ) extends Mapping[T] with ObjectMapping[T] {

  val field1 = f1._2.withPrefix(f1._1).withPrefix(path)
  val field2 = f2._2.withPrefix(f2._1).withPrefix(path)
  val field3 = f3._2.withPrefix(f3._1).withPrefix(path)
  val field4 = f4._2.withPrefix(f4._1).withPrefix(path)
  val field5 = f5._2.withPrefix(f5._1).withPrefix(path)
  val field6 = f6._2.withPrefix(f6._1).withPrefix(path)
  val field7 = f7._2.withPrefix(f7._1).withPrefix(path)

  override def validate(t: T): Seq[FormError] =
    validateSelf(t) ++
      unapply(t).fold[Seq[FormError]](Seq.empty) {
        case (val1, val2, val3, val4, val5, val6, val7) =>
          field1.validate(val1) ++ field2.validate(val2) ++ field3.validate(val3) ++ field4.validate(val4) ++ field5.validate(val5) ++ field6.validate(val6) ++ field7.validate(val7)
      }

  override def map(t: T): Map[String, Any] =
    unapply(t).fold[Map[String, Any]](
      Map.empty
    ) {
      case (val1, val2, val3, val4, val5, val6, val7) => field1.map(val1) ++ field2.map(val2) ++ field3.map(val3) ++ field4.map(val4) ++ field5.map(val5) ++ field6.map(val6) ++ field7.map(val7)
    }

  override def unmap(data: Map[String, Any]): Either[Seq[FormError], T] =
    merge(field1.unmap(data), field2.unmap(data), field3.unmap(data), field4.unmap(data), field5.unmap(data), field6.unmap(data), field7.unmap(data)).right.flatMap(fs =>
      unmapAndValidate(apply(
        fs(0).asInstanceOf[F1],
        fs(1).asInstanceOf[F2],
        fs(2).asInstanceOf[F3],
        fs(3).asInstanceOf[F4],
        fs(4).asInstanceOf[F5],
        fs(5).asInstanceOf[F6],
        fs(6).asInstanceOf[F7]
      ))
    )

  override def withPrefix(prefix: String): Mapping[T] = copy(path = addPrefix(prefix))

  override def verifying(additional: Constraint[T]*): Mapping[T] =
    copy(constraints = constraints ++ additional)

}