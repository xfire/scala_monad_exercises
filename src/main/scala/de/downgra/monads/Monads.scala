package de.downgra.monads

// 1. Start here. Observe this trait
trait Monad[M[_]] {
  def flatMap[A, B](a: M[A], f: A => M[B]): M[B]
  def unital[A](a: A): M[A]
}
 
// A simple data type, which turns out to satisfy the above trait
case class Inter[A](f: Int => A)
 
// So does this.
case class Identity[A](a: A)
 
// Monad implementations
object Monad {

  def ListMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](a: List[A], f: A => List[B]): List[B] = a.foldLeft(List[B]())((acc, v) => acc ++ f(v))
    def unital[A](a: A): List[A] = List(a)
  }
 
  def OptionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](a: Option[A], f: A => Option[B]): Option[B] = a match {
      case Some(x) => f(x)
      case None => None
    }
    def unital[A](a: A): Option[A] = Some(a)
  }
 
  def InterMonad: Monad[Inter] = new Monad[Inter] {
    def flatMap[A, B](a: Inter[A], f: A => Inter[B]): Inter[B] = Inter(i => f(a.f(i)).f(i))
    def unital[A](a: A): Inter[A] = Inter(_ => a)
  }
 
  def IdentityMonad: Monad[Identity] = new Monad[Identity] {
    def flatMap[A, B](a: Identity[A], f: A => Identity[B]): Identity[B] = f(a.a)
    def unital[A](a: A): Identity[A] = Identity(a)
  }

}
 
object MonadicFunctions {

  def sequenceM[M[_], A](as: List[M[A]], m: Monad[M]): M[List[A]] =
    as.foldRight(m.unital(List[A]()))(
      (v, acc) => m.flatMap(v,
        (x: A) => m.flatMap(acc,
          (ys: List[A]) => m.unital(x :: ys))))
 
  def fmap[M[_], A, B](a: M[A], f: A => B, m: Monad[M]): M[B] =
    m.flatMap(a, (x: A) => m.unital(f(x)))
 
  def flattenM[M[_], A](a: M[M[A]], m: Monad[M]): M[A] =
    m.flatMap(a, (x: M[A]) => x)
 
  def applyM[M[_], A, B](f: M[A => B], a: M[A], m: Monad[M]): M[B] =
    m.flatMap(f,
      (ff: A => B) => m.flatMap(a,
        (x: A) => m.unital(ff(x))))
 
  def filterM[M[_], A](f: A => M[Boolean], as: List[A], m: Monad[M]): M[List[A]] = as match {
    case Nil => m.unital(List[A]())
    case x :: xs =>
      m.flatMap(f(x),
        (p: Boolean) => m.flatMap(filterM(f, xs, m),
          (ys: List[A]) => m.unital(if(p) x :: ys else ys)))
  }
 
  def replicateM[M[_], A](n: Int, a: M[A], m: Monad[M]): M[List[A]] = n match {
    case 0 => m.unital(List[A]())
    case _ => 
      m.flatMap(a, 
        (v: A) => m.flatMap(replicateM(n - 1, a, m), 
          (xs: List[A]) => m.unital(v :: xs)))
  }
 
  def liftM2[M[_], A, B, C](f: (A, B) => C, a: M[A], b: M[B], m: Monad[M]): M[C] =
    m.flatMap(a,
      (x: A) => m.flatMap(b,
        (y: B) => m.unital(f(x, y))))
 
  def liftM3[M[_], A, B, C, D](f: (A, B, C) => D, a: M[A], b: M[B], c: M[C], m: Monad[M]): M[D] =
    m.flatMap(a,
      (x: A) => m.flatMap(b,
        (y: B) => m.flatMap(c,
          (z: C) => m.unital(f(x, y, z)))))

  def foldM[M[_], A, B](f: (A, B) => M[A], a: A, xs: List[B], m: Monad[M]): M[A] =
    xs.foldLeft(m.unital(a))((acc: M[A], v: B) =>
      m.flatMap(acc,
        (x: A) => f(x, v)))

}
 
// vim: set ts=2 sw=2 et:
