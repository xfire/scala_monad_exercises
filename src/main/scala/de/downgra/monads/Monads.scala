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
  // 2. Replace error("todo") with an implementation
  def ListMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](a: List[A], f: A => List[B]): List[B] = a.foldLeft(List[B]())((acc, v) => acc ++ f(v))
    def unital[A](a: A): List[A] = List(a)
  }
 
  // 3. Replace error("todo") with an implementation
  def OptionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](a: Option[A], f: A => Option[B]): Option[B] = a match {
      case Some(x) => f(x)
      case None => None
    }
    def unital[A](a: A): Option[A] = Some(a)
  }
 
  // 4. Replace error("todo") with an implementation
  def InterMonad: Monad[Inter] = new Monad[Inter] {
    def flatMap[A, B](a: Inter[A], f: A => Inter[B]): Inter[B] = Inter(i => f(a.f(i)).f(i))
    def unital[A](a: A): Inter[A] = Inter(_ => a)
  }
 
  // 5. Replace error("todo") with an implementation
  def IdentityMonad: Monad[Identity] = new Monad[Identity] {
    def flatMap[A, B](a: Identity[A], f: A => Identity[B]): Identity[B] = f(a.a)
    def unital[A](a: A): Identity[A] = Identity(a)
  }
}
 
object MonadicFunctions {
  // 6. Replace error("todo") with an implementation
  def sequence[M[_], A](as: List[M[A]], m: Monad[M]): M[List[A]] = {
    as.foldRight(m.unital(List[A]()))((v, acc) => m.flatMap(v, (x: A) => m.flatMap(acc, (y: List[A]) => m.unital(x :: y))))
  }
 
  // 7. Replace error("todo") with an implementation
  def fmap[M[_], A, B](a: M[A], f: A => B, m: Monad[M]): M[B] =
    error("todo")
 
  // 8. Replace error("todo") with an implementation
  def flatten[M[_], A](a: M[M[A]], m: Monad[M]): M[A] =
    error("todo")
 
  // 9. Replace error("todo") with an implementation
  def apply[M[_], A, B](f: M[A => B], a: M[A], m: Monad[M]): M[B] =
    error("todo")
 
  // 10. Replace error("todo") with an implementation
  def filterM[M[_], A](f: A => M[Boolean], as: List[A]
    , m: Monad[M]): M[List[A]] =
    error("todo")
 
  // 11. Replace error("todo") with an implementation
  def replicateM[M[_], A](n: Int, a: M[A], m: Monad[M]): M[List[A]] =
    error("todo: flatMap n times to produce a list")
 
  // 12. Replace error("todo") with an implementation
  def lift2[M[_], A, B, C](f: (A, B) => C, a: M[A], b: M[B]
    , m: Monad[M]): M[C] =
    error("todo")
 
  // lift3, lift4, etc. Interesting question: Can we have liftN?
}
 
object Main {
  def main(args: Array[String]) {
    import Monad._
    import MonadicFunctions._
 
    val plusOne = Inter(1+)
    val multTwo = Inter(2*)
    val squared = Inter(n => n*n)
    val plus = (_: Int) + (_: Int)
 
    val values = List(
      // sequence
      sequence(List(List(1, 2), List(3, 4)), ListMonad),
      sequence(List(Some(7), Some(8), Some(9)), OptionMonad),
      sequence(List(Some(7), None, Some(9)), OptionMonad),
      sequence(List(plusOne, multTwo, squared), InterMonad) f 7,
      sequence(List(Identity(7), Identity(4)), IdentityMonad) //,
      // fmap
      /*
       * fmap(List(1, 2, 3), (x: Int) => x * 10, ListMonad),
       * fmap(Some(8), (x: Int) => x * 10, OptionMonad),
       * fmap(None: Option[Int], (x: Int) => x * 10, OptionMonad),
       * fmap(plusOne, (x: Int) => x * 10, InterMonad) f 7,
       * fmap(Identity(9), (x: Int) => x * 10, IdentityMonad),
       * // flatten
       * flatten(List(List(1, 2), List(3, 4)), ListMonad),
       * flatten(Some(Some(8)), OptionMonad),
       * flatten(Some(None: Option[Int]), OptionMonad),
       * flatten(None: Option[Option[Int]], OptionMonad),
       * flatten(Inter(a => Inter(a *)), InterMonad) f 7,
       * flatten(Identity(Identity(8)), IdentityMonad),
       * // apply
       * apply(List((a: Int) => a + 1,
       *            (a: Int) => a * 2,
       *            (a: Int) => a % 2), List(1, 2, 3), ListMonad),
       * apply(Some((a: Int) => a + 1), Some(8), OptionMonad),
       * apply(None: Option[Int => Int], Some(8), OptionMonad),
       * apply(Some((a: Int) => a + 1), None: Option[Int], OptionMonad),
       * apply(Inter(a => (b: Int) => a * b), Inter(1+), InterMonad) f 7,
       * apply(Identity((a: Int) => a + 1), Identity(7), IdentityMonad),
       * // filterM
       * filterM((a: Int) => List(a > 2, a % 2 == 0), List(1, 2, 3), ListMonad),
       * filterM((a: Int) => Some(a > 1), List(1, 2, 3), OptionMonad),
       * filterM((a: Int) => Inter(n => a * n % 2 == 0),
       *   List(1, 2, 3), InterMonad) f 7,
       * filterM((a: Int) => Identity(a > 1), List(1, 2, 3), IdentityMonad),
       * // replicateM
       * replicateM(2, List(7, 8), ListMonad),
       * replicateM(2, Some(7), OptionMonad),
       * replicateM(2, plusOne, InterMonad) f 7,
       * replicateM(2, Identity(6), IdentityMonad),
       * // lift2
       * lift2(plus, List(1, 2), List(3, 4), ListMonad),
       * lift2(plus, Some(7), Some(8), OptionMonad),
       * lift2(plus, Some(7), None: Option[Int], OptionMonad),
       * lift2(plus, None: Option[Int], Some(8), OptionMonad)
       */
    )
 
    val verify = List(
      // sequence
      List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)),
      Some(List(7, 8, 9)),
      None,
      List(8, 14, 49),
      Identity(List(7, 4)),
      // fmap
      List(10, 20, 30),
      Some(80),
      None,
      80,
      Identity(90),
      // flatten
      List(1, 2, 3, 4),
      Some(8),
      None,
      None,
      49,
      Identity(8),
      // apply
      List(2, 3, 4, 2, 4, 6, 1, 0, 1),
      Some(9),
      None,
      None,
      56,
      Identity(8),
      // filterM
      List(List(3), Nil, List(2, 3), List(2), List(3),
        Nil, List(2, 3), List(2)),
      Some(List(2, 3)),
      List(2),
      Identity(List(2, 3)),
      // replicateM
      List(List(7, 7), List(7, 8), List(8, 7), List(8, 8)),
      Some(List(7, 7)),
      List(8, 8),
      Identity(List(6, 6)),
      // lift2
      List(4, 5, 5, 6),
      Some(15),
      None,
      None
    )
 
    for((a, b) <- values zip verify) {
      // println(a)
      // println(b)
      println(if(a == b) "PASS"
              else "FAIL. Expected: " + b + " Actual: " + a)
    }
  }
}

// vim: set ts=2 sw=2 et:
