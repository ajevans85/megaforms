package megaforms

import scala.collection.immutable.Seq
import scala.reflect.ClassTag

/**
 * Abstraction to talk about the state of one specific field in the form, integration point
 * against react from the components side - they may know about this class, but probably no
 * other form api class.
 */
final class Field private[megaforms](val form: Form[_], val path: String) {

  /** indices that has got values if this is a field containing a sequence of values */
  def indexes: Seq[Int] = FormData.indexesFor(path, form.data)
  def errors: Seq[String] = form.errors(path)

  /** raw access to the type-unsafe field value, which might be missing */
  def data: Option[Any] = form.data.get(path)

  /** type safe:ish access to the actual value of the path of the form, if it is undefined
    * ```orElse``` is used. If the value is not of type ```T``` a runtime exception is thrown
    */
  def value[T: ClassTag](orElse: => T): T =
    data.fold(
      orElse
    ) {
      case t: T => t
      case x =>
        val tpe = implicitly[ClassTag[T]]
        throw new RuntimeException(s"Tried to read $path as a ${tpe.runtimeClass.getName} but was value $x of type ${x.getClass}")
    }

  /** return a sub-field of this */
  def apply(subkey: String): Field = form(FormData.keyWithChild(path, subkey))
  /** return an indexed sub-field of this */
  def apply(index: Int): Field = form(FormData.keyWithIndex(path, index))


  override def hashCode(): Int = form.hashCode + path.hashCode

  override def equals(other: Any) = other match {
    case f: Field => f.form.eq(form) && f.path == path
    case _ => false
  }

  override def toString = s"Field($path, $form)"
}