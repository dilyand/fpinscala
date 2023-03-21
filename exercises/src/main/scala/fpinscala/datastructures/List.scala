package fpinscala.datastructures

import fpinscala.datastructures.List.length

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, z)((a, b) => f(b, a))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("head of empty list")
      case Cons(_, t) => Cons(h, t)
    }

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => sys.error("drop on empty list")
      case Cons(_, t) => drop(t, n - 1)
    }

  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, y) => 1 + y)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b, a) => f(a, b))

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((y, x) => 1 + y)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((b, a) => Cons(a, b))

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  def append3[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l2), l1)((b, a) => Cons(a, b))

  def flatten[A](ls: List[List[A]]): List[A] =
    foldLeft(ls, List[A]())((acc, l) => append3(acc, l))

  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1(t))
  }

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def doubleToString2(l: List[Double]): List[String] =
    foldLeft(reverse(l), List[String]())((b, a) => Cons(a.toString, b))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def map2[A, B](l: List[A])(f: A => B): List[B] =
    foldLeft(reverse(l), List[B]())((b, a) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) {
      Cons(h, t)
    } else {
      t
    })

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(reverse(as), List[A]())((b, a) => if (f(a)) {
      Cons(a, b)
    } else {
      b
    })

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map2(as)(f))

  def filter3[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addLists(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, _) => l2
      case (_, Nil) => l1
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] =
    (l1, l2) match {
      case (Nil, _) => l2
      case (_, Nil) => l1
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  @tailrec
  def takeWhile[A](l: List[A], f: List[A] => Boolean, acc: List[A] = Nil): List[A] = l match {
    case Cons(h, t) if f(Cons(h, acc)) => takeWhile(t, f, Cons(h, acc))
    case _ => reverse(acc)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val subLength = length(sub)

    @tailrec
    def go(sup: List[A], sub: List[A]): Boolean = {
      val init = takeWhile(sup, length(_) <= subLength)
      init match {
        case Nil => false
        case _ => if (init == sub) true else go(drop(sup, 1), sub)
      }
    }

    go(sup, sub)
  }
}
