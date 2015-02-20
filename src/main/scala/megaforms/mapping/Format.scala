package megaforms.mapping

import scala.math.BigDecimal.RoundingMode
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.Seq

/**
 * A format knows how to transform a value to a form value and back, never nested always the end transformation
 * also, no validation/constraints here except that an input value _can_ be transformed to a given type.
 *
 * @tparam T the type of scala value the format handles
 */
trait Format[T] {
  /** transform the real value into something that the a form field can handle */
  def format(t: T): Any
  /** @return Right(parsed value) or Left("error description") */
  def parse(formValue: Any): Either[String, T]
}

object Formats {

  class Identity[T](implicit tag: Manifest[T]) extends Format[T] {
    override def format(t: T): Any = t

    /** @return Right(parsed value) or Left("error description") */
    override def parse(formValue: Any): Either[String, T] = formValue match {
      case t: T => Right(t)
      case x => Left(s"Unexpected type of value ${x.getClass}")
    }
  }

  object Text extends Format[String] {
    override def format(t: String): Any = t

    override def parse(formValue: Any): Either[String, String] = formValue match {
      case text: String => Right(text)
      case x => Left(s"Unexpected type of value $x, expected a String")
    }
  }

  object Integer extends Format[Int] {
    override def format(t: Int): String = t.toString

    override def parse(value: Any): Either[String, Int] = value match {
      case text: String =>
        Try(text.toInt) match {
          case Success(int) => Right(int)
          case Failure(ex) => Left(s"Value '$text' is not a number")
        }

      case n: Int => Right(n)
      case x => Left(s"Value $x is not a number")

    }
  }

  /** loss less big decimal representation of decimal numbers */
  case class Decimal(decimals: Int) extends Format[BigDecimal] {

    override def format(t: BigDecimal): String =
      // make sure we get the right amount of literals
      if (t.scale != decimals) t.setScale(decimals).toString()
      else t.toString()

    override def parse(value: Any): Either[String, BigDecimal] = value match {
      case text: String =>
        Try(
          BigDecimal(text)
            // truncate additional decimals
            .setScale(decimals, RoundingMode.DOWN)
        ) match {
          case Success(value) => Right(value)
          case Failure(ex) => Left(s"Value $value is not a number")
        }
      case b: BigDecimal => Right(b)
      case x => Left(s"Value $value is not a number")

    }

  }

  object Boolean extends Format[Boolean] {
    override def format(t: Boolean): Any = t
    override def parse(formValue: Any): Either[String, Boolean] = formValue match {
      case b: Boolean => Right(b)
      case "true" => Right(true)
      case "false" => Right(false)
      case x => Left(s"Unexpected type of value for boolean field $x")
    }
  }

  /** a format that will always return the given value */
  def ignored[T](value: T): Format[T] = new Format[T]{
    override def format(t: T): Any = value
    override def parse(formValue: Any): Either[String, T] = Right(value)
  }

  /** a format that will keep the value intact but pass it through */
  def keep[T]: Format[T] = new Format[T]{
    override def format(t: T): Any = t
    override def parse(formValue: Any): Either[String, T] =
      Try(formValue.asInstanceOf[T]) match {
        case Success(t) => Right(t)
        case Failure(ex) => Left(ex.getMessage)
      }
  }

}