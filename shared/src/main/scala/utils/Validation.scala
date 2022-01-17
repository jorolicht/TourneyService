package shared.utils

import scala.util.{Try, Success, Failure}

object Validation {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case FlatMap(sub, cont) => FlatMap(sub, cont andThen (_ flatMap f))
    }

    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A]) extends Free[F, A]

  implicit def liftF[F[_], A](fa: F[A]): Free[F, A] = FlatMap(fa, Return.apply)

  trait Validator[A] {
    def validate: Option[Error]
    def unbox: A
  }

  sealed trait Executor[F[_]] {
    def exec[A](fa: F[A]): Option[Error]
    def unbox[A](fa: F[A]): A
  }

  val interpreter  = new Executor[Validator] {
    override def unbox[A](fa: Validator[A]) = fa.unbox
    override def exec[A](fa: Validator[A]) = fa.validate
  }

  def validate[F[_], A](prg: Free[F, A], interpreter : Executor[F]): List[Error] = {
    def go(errorList: List[Option[Error]], prg: Free[F, A]): List[Option[Error]]= prg match {
      case Return(a) => errorList
      case FlatMap(sub, cont) => go(interpreter.exec(sub) :: errorList, cont(interpreter.unbox(sub)))
    }
    go(List.empty[Option[Error]], prg).flatten
  }
}