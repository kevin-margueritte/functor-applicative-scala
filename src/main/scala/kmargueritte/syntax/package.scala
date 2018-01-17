package kmargueritte

package object syntax {

  implicit class FunctorImpl[F[_], A](fa: F[A])(implicit functor: Functor[F]) {
    def map[B](f: A => B) = functor.map(fa)(f)
  }

  implicit class ApplicativeImpl[F[_], A](fa: F[A])(implicit applicative: Applicative[F]) {
    def map[B](f: A => B) = applicative.map(fa)(f)
  }

  implicit class Map2[F[_], A](fa: F[A])(implicit applicative: Applicative[F]) {
    def map2[B, Z](fb: F[B])(f: (A,B) => Z) = applicative.map2(fa, fb)(f)
  }
}
