package kmargueritte.option

import java.util.NoSuchElementException

import kmargueritte.{Applicative, Functor}

sealed abstract class Maybe[+A] {

  def isEmpty: Boolean

  def get: A

  final def getOrElse[B >: A](value: B) = {
    this.isEmpty match {
      case true => value
      case _ => this.get
    }
  }

}

object Maybe {

  def apply[A](a: A): Maybe[A] = {
    if (a == null)
      Nope
    else
      Tadam(a)
  }

}

final case class Tadam[+A](value: A) extends Maybe[A] {

  override def get: A = value
  override def isEmpty: Boolean = false

}

final object Nope extends Maybe[Nothing] {

  override def get: Nothing = throw new NoSuchElementException("No value")
  override def isEmpty: Boolean = true

}

object functor {

  implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {

    override def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match {
      case Tadam(value) => Tadam(f(value))
      case _ => Nope
    }

  }

}

object applicative {

  implicit val maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {

    override def ap[A, B](ff: Maybe[A => B])(fa: Maybe[A]): Maybe[B] = fa match {
      case Tadam(value) => ff match {
        case Tadam(f) => Tadam(f(value))
        case _ => Nope
      }
      case _  => Nope
    }

    override def pure[A](a: A): Maybe[A] = Maybe(a)

    override def product[A,B](fa: Maybe[A], fb: Maybe[B]): Maybe[(A,B)] = (fa, fb) match {
      case (Tadam(a), Tadam(b)) => Maybe((a,b))
      case _ => Nope
    }
  }

}
