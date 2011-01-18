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
  def sequence[M[_], A](as: List[M[A]], m: Monad[M]): M[List[A]] = {
    as.foldRight(m.unital(List[A]()))((v, acc) => m.flatMap(v, (x: A) => m.flatMap(acc, (y: List[A]) => m.unital(x :: y))))
  }
 
  def fmap[M[_], A, B](a: M[A], f: A => B, m: Monad[M]): M[B] = m.flatMap(a, ((x: A) => m.unital(f(x))))
 
  // 8. Replace error("todo") with an implementation
  def flatten[M[_], A](a: M[M[A]], m: Monad[M]): M[A] = error("todo")
 
  // 9. Replace error("todo") with an implementation
  def apply[M[_], A, B](f: M[A => B], a: M[A], m: Monad[M]): M[B] = error("todo")
 
  // 10. Replace error("todo") with an implementation
  def filterM[M[_], A](f: A => M[Boolean], as: List[A], m: Monad[M]): M[List[A]] = error("todo")
 
  // 11. Replace error("todo") with an implementation
  def replicateM[M[_], A](n: Int, a: M[A], m: Monad[M]): M[List[A]] = error("todo: flatMap n times to produce a list")
 
  // 12. Replace error("todo") with an implementation
  def lift2[M[_], A, B, C](f: (A, B) => C, a: M[A], b: M[B], m: Monad[M]): M[C] = error("todo")
 
  // lift3, lift4, etc. Interesting question: Can we have liftN?
}
 
// vim: set ts=2 sw=2 et:
