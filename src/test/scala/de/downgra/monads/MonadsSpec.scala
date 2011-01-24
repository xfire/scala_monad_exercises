package de.downgra.monads

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class MonadSpec extends FunSuite with ShouldMatchers {

  import Monad._
  import MonadicFunctions._

  val plusOne = Inter(1+)
  val multTwo = Inter(2*)
  val squared = Inter(n => n*n)
  val plus = (_: Int) + (_: Int)
  val plus3 = (_: Int) + (_: Int) + (_: Int)

  // sequence --------------------------------

  test("sequence on ListMonad") {
    sequenceM(List(List(1, 2), List(3, 4)), ListMonad) should be (List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))
  }

  test("sequence on OptionMonad") {
    sequenceM(List(Some(7), Some(8), Some(9)), OptionMonad) should be (Some(List(7, 8, 9)))
    sequenceM(List(Some(7), None, Some(9)), OptionMonad) should be (None)
  }

  test("sequence on InterMonad") {
    (sequenceM(List(plusOne, multTwo, squared), InterMonad) f 7) should be (List(8, 14, 49))
  }

  test("sequence on IdentityMonad") {
    sequenceM(List(Identity(7), Identity(4)), IdentityMonad) should be (Identity(List(7, 4)))
  }

  // fmap --------------------------------

  test("fmap on ListMonad") {
    fmap(List(1, 2, 3), (x: Int) => x * 10, ListMonad) should be (List(10, 20, 30))
  }

  test("fmap on OptionMonad") {
    fmap(Some(8), (x: Int) => x * 10, OptionMonad) should be (Some(80))
    fmap(None: Option[Int], (x: Int) => x * 10, OptionMonad) should be (None)
  }

  test("fmap on InterMonad") {
    (fmap(plusOne, (x: Int) => x * 10, InterMonad) f 7) should be (80)
  }

  test("fmap on IdentityMonad") {
    fmap(Identity(9), (x: Int) => x * 10, IdentityMonad) should be (Identity(90))
  }

  // flatten --------------------------------

  test("flatten on ListMonad") {
   flattenM(List(List(1, 2), List(3, 4)), ListMonad) should be (List(1, 2, 3, 4))
  }

  test("flatten on OptionMonad") {
   flattenM(Some(Some(8)), OptionMonad) should be (Some(8))
   flattenM(Some(None: Option[Int]), OptionMonad) should be (None)
   flattenM(None: Option[Option[Int]], OptionMonad) should be (None)
  }

  test("flatten on InterMonad") {
   (flattenM(Inter(a => Inter(a *)), InterMonad) f 7) should be (49)
  }

  test("flatten on IdentityMonad") {
   flattenM(Identity(Identity(8)), IdentityMonad) should be (Identity(8))
  }

  // apply --------------------------------

  test("apply on ListMonad") {
     applyM(List((a: Int) => a + 1,
                 (a: Int) => a * 2,
                 (a: Int) => a % 2), List(1, 2, 3), ListMonad) should be (List(2, 3, 4, 2, 4, 6, 1, 0, 1))
  }

  test("apply on OptionMonad") {
   applyM(Some((a: Int) => a + 1), Some(8), OptionMonad) should be (Some(9))
   applyM(None: Option[Int => Int], Some(8), OptionMonad) should be (None)
   applyM(Some((a: Int) => a + 1), None: Option[Int], OptionMonad) should be (None)
  }

  test("apply on InterMonad") {
   (applyM(Inter(a => (b: Int) => a * b), Inter(1+), InterMonad) f 7) should be (56)
  }

  test("apply on IdentityMonad") {
   applyM(Identity((a: Int) => a + 1), Identity(7), IdentityMonad) should be (Identity(8))
  }

  // filterM --------------------------------

  test("filterM on ListMonad") {
   filterM((a: Int) => List(a > 2, a % 2 == 0), List(1, 2, 3), ListMonad) should be (
     List(List(3), Nil, List(2, 3), List(2), List(3), Nil, List(2, 3), List(2)))
  }

  test("filterM on OptionMonad") {
   filterM((a: Int) => Some(a > 1), List(1, 2, 3), OptionMonad) should be (Some(List(2, 3)))
  }

  test("filterM on InterMonad") {
   (filterM((a: Int) => Inter(n => a * n % 2 == 0), List(1, 2, 3), InterMonad) f 7) should be (
     List(2))
  }

  test("filterM on IdentityMonad") {
   filterM((a: Int) => Identity(a > 1), List(1, 2, 3), IdentityMonad) should be (Identity(List(2, 3)))
  }

  // replicateM --------------------------------

  test("replicateM on ListMonad") {
   replicateM(2, List(7, 8), ListMonad) should be (List(List(7, 7), List(7, 8), List(8, 7), List(8, 8)))
  }

  test("replicateM on OptionMonad") {
   replicateM(2, Some(7), OptionMonad) should be (Some(List(7, 7)))
  }

  test("replicateM on InterMonad") {
   (replicateM(2, plusOne, InterMonad) f 7) should be (List(8, 8))
  }

  test("replicateM on IdentityMonad") {
   replicateM(2, Identity(6), IdentityMonad) should be (Identity(List(6, 6)))
  }

  // lift2 --------------------------------

  test("lift2 on ListMonad") {
   liftM2(plus, List(1, 2), List(3, 4), ListMonad) should be (List(4, 5, 5, 6))
  }

  test("lift2 on OptionMonad") {
   liftM2(plus, Some(7), Some(8), OptionMonad) should be (Some(15))
   liftM2(plus, Some(7), None: Option[Int], OptionMonad) should be (None)
   liftM2(plus, None: Option[Int], Some(8), OptionMonad) should be (None)
  }

  // lift3 --------------------------------

  test("lift3 on ListMonad") {
    liftM3(plus3, List(1, 2), List(3, 4), List(5, 6), ListMonad) should be (List(9, 10, 10, 11, 10, 11, 11, 12))
  }

  test("lift3 on OptionMonad") {
   liftM3(plus3, Some(7), Some(8), Some(9), OptionMonad) should be (Some(24))
   liftM3(plus3, None: Option[Int], Some(8), Some(9), OptionMonad) should be (None)
   liftM3(plus3, Some(7), None: Option[Int], Some(9), OptionMonad) should be (None)
   liftM3(plus3, Some(7), Some(8), None: Option[Int], OptionMonad) should be (None)
  }

  // fold ----------------------------------

  test("fold on ListMonad") {
    foldM((acc: Int, v: Int) => List(v + acc), 0, List(1, 2, 3, 4, 5), ListMonad) should be (List(15))
  }

  test("fold on OptionMonad") {
    val -/- = (a: Double, b: Int) => b match {
      case 0 => None
      case _ => Some(a / b)
    }
    foldM(-/-, 1.0, List(1, 2), OptionMonad) should be (Some(0.5))
    foldM(-/-, 1.0, List(1, 0, 2), OptionMonad) should be (None)
  }

  test("fold on InterMonad") {
    val w = (a: List[Int], b: Int) => Inter((p: Int) => if(p >= b) b :: a else a) 
    (foldM(w, List(), List(1,2,3,4), InterMonad) f 2) should be (List(2, 1))
    (foldM(w, List(), List(1,2,3,4), InterMonad) f 4) should be (List(4, 3, 2, 1))
  }

  test("fold on IdentityMonad") {
    foldM((acc: Int, v: Int) => Identity(acc + v), 0, List(1, 2, 3, 4, 5), IdentityMonad) should be (Identity(15))
  }

}

// vim: set ts=2 sw=2 et:
