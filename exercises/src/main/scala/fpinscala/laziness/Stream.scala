package fpinscala.laziness

import Stream._
trait Stream[+A] {

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

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = {
    def go(str: Stream[A], acc: List[A]): List[A] = str match {
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => acc
    }
    go(this, List()).reverse
  }

  // Currently works in reverse. When reverse or foldLeft are implemented, will make it work.
  def take(n: Int): Stream[A] = {
    def go(n: Int, s: Stream[A], acc: Stream[A]):  Stream[A] = n match {
      case 0 => acc
      case _ => s match {
        case Empty => acc
        case Cons(h, t) => go(n - 1, t(), Cons(h, () => acc))
      }
    }
    go(n, this, empty)
  }

//  def takeWhile(p: A => Boolean): Stream[A] = this match {
//    case Empty => Empty
//    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
//  }

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = this.foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this.foldRight(None: Option[A])((h, t) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this.foldRight(empty[B])((a, b) => cons(f(a), b))

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B])((a, b) => f(a) append b)

  def filter(f: A => Boolean): Stream[A] = this.foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def main(args: Array[String]): Unit = {
    print(unfold(0)(x => Some((x, x + 1))).take(3).toList2)
  }
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
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  val fibs: Stream[Int] = {
    def go(i: Int, j: Int): Stream[Int] = {
      cons(j, go(i + j, i))
    }
    go(1, 0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => empty
  }
}