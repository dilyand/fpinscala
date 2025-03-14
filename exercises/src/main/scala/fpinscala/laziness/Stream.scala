package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {
  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A] = List.empty[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(this).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
    case (Cons(h, _), 1) => Some((h(), (empty, 0)))
    case _ => None
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = this.foldRight(empty[A]) { (a, b) => if (p(a)) cons(a, b) else empty }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true) { (a, b) => p(a) && b }

  def headOption: Option[A] = this.foldRight(Option.empty[A]) { (a, _) => Some(a) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this.foldRight(empty[B]) { (a, b) => cons(f(a), b) }

  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def filter(p: A => Boolean): Stream[A] = this.foldRight(empty[A]) { (a, b) => if (p(a)) cons(a, b) else b }

  def append[B >: A](s: => Stream[B]): Stream[B] = this.foldRight(s) { (a, b) => cons(a, b) }

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B]) { (a, b) => f(a).append(b) }

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def loop(prev: Int, curr: Int): Stream[Int] = Stream.cons(prev, loop(curr, prev + curr))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((element, state)) => Stream.cons(element, unfold(state)(f))
    case None => Stream.empty[A]
  }

  val fibsViaUnfold: Stream[Int] = unfold((0, 1)) { case (p, n) => Some((p, (p, p + n))) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Some((a, a)))

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
}
