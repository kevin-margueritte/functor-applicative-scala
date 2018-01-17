package kmargueritte

trait Applicative[F[_]] extends Functor[F] {

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = map(product(fa, fb))(f.tupled)
}
