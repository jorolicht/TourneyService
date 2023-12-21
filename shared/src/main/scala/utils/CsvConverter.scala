package shared.utils

import shapeless._
import scala.collection.immutable.{:: => Cons}
import scala.util.{Try,Success,Failure}


/** Exception to throw if something goes wrong during CSV parsing */
class CSVException(s: String) extends RuntimeException(s)

/** Trait for types that can be serialized to/deserialized from CSV */
trait CSVConverter[T] {
  def from(s: String): Try[T]
  def to(t: T): String
}

/** Instances of the CSVConverter trait */
object CSVConverter {
  def apply[T](implicit st: => CSVConverter[T]): CSVConverter[T] = st

  def fail(s: String) = Failure(new CSVException(s))


  // Primitives
  implicit def stringCSVConverter: CSVConverter[String] = new CSVConverter[String] {
    def from(s: String): Try[String] = Success(s)
    def to(s: String): String = s
  }

  implicit def intCsvConverter: CSVConverter[Int] = new CSVConverter[Int] {
    def from(s: String): Try[Int] = Try(s.toInt)
    def to(i: Int): String = i.toString
  }

  implicit def longCsvConverter: CSVConverter[Long] = new CSVConverter[Long] {
    def from(s: String): Try[Long] = Try(s.toLong)
    def to(l: Long): String = l.toString
  }

  implicit def boolCSVConverter: CSVConverter[Boolean] = new CSVConverter[Boolean] {
    def from(s: String): Try[Boolean] = Try(s.toBoolean)
    def to(b: Boolean): String = b.toString
  }


  def listCsvLinesConverter[A](l: List[String])(implicit ec: CSVConverter[A])
      : Try[List[A]] = l match {
    case Nil => Success(Nil)
    case Cons(s,ss) => for {
        x <- ec.from(s)
        xs <- listCsvLinesConverter(ss)(ec)
      } yield Cons(x, xs)
  }

  implicit def listCsvConverter[A](implicit ec: CSVConverter[A])
      : CSVConverter[List[A]] = new CSVConverter[List[A]] {
    def from(s: String): Try[List[A]] = listCsvLinesConverter(s.split("\n").toList)(ec)
    def to(l: List[A]): String = l.map(ec.to).mkString("\n")
  }


  // HList
  implicit def deriveHNil: CSVConverter[HNil] =
    new CSVConverter[HNil] {
      def from(s: String): Try[HNil] = s match {
        case "" => Success(HNil)
        case s => fail("Cannot convert '" ++ s ++ "' to HNil")
      }
      def to(n: HNil) = ""
    }
  
  implicit def deriveHCons[V, T <: HList]
    (implicit scv: => CSVConverter[V], sct: => CSVConverter[T])
        : CSVConverter[V :: T] =
      new CSVConverter[V :: T] {

        def from(s: String): Try[V :: T] = s.span(_ != '·') match {
          case (before,after) =>
            for {
              front <- scv.from(before)
              back <- sct.from(if (after.isEmpty) after else after.tail)
            } yield front :: back

          case _ => fail("Cannot convert '" ++ s ++ "' to HList")
        }

        def to(ft: V :: T): String = {
          scv.to(ft.head) ++ "·" ++ sct.to(ft.tail)
        }
      }

  implicit def deriveHConsOption[V, T <: HList]
  (implicit scv: => CSVConverter[V], sct: => CSVConverter[T])
  : CSVConverter[Option[V] :: T] =
    new CSVConverter[Option[V] :: T] {

      def from(s: String): Try[Option[V] :: T] = s.span(_ != '·') match {
        case (before,after) =>
          (for {
            front <- scv.from(before)
            back <- sct.from(if (after.isEmpty) after else after.tail)
          } yield Some(front) :: back).orElse {
            sct.from(if (s.isEmpty) s else s.tail).map(None :: _)
          }

        case _ => fail("Cannot convert '" ++ s ++ "' to HList")
      }

      def to(ft: Option[V] :: T): String = {
        ft.head.map(scv.to(_) ++ "·").getOrElse("") ++ sct.to(ft.tail)
      }
    }


  // Anything with a Generic

  implicit def deriveClass[A,R](implicit gen: Generic.Aux[A,R], conv: CSVConverter[R])
      : CSVConverter[A] = new CSVConverter[A] {
    
    def from(s: String): Try[A] = conv.from(s).map(gen.from)
    def to(a: A): String = conv.to(gen.to(a))
  }
}
