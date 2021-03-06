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

  def take(n: Int): Stream[A] = {
    unfold((this, n)){
      case (_, 0) => None
      case (Empty, n) => None
      case (Cons(h, t), n) => Some(h(), (t(), n - 1))
      case _ => None
    }
  }

//  def takeWhile(p: A => Boolean): Stream[A] = this match {
//    case Empty => Empty
//    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
//  }

  def drop(n: Int): Stream[A] = n match {
    case 0 => this
    case _ => this match {
      case Cons(h, t) => t() drop n-1
      case _ => empty
    }
  }

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

//  def startsWith[B](s: Stream[B]): Boolean = ???

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this)((s) => s match {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) => if (p(h())) Some((h(), t())) else None
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)){
    case (Cons(x, tail1), Cons(y, tail2)) => Some(f(x(), y()), (tail1(), tail2()))
    case _ => None
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case _ => None
  }
  def startsWith[A](s2: Stream[A]): Boolean = {
    this.zipWith(s2)((_, _)).foldRight(true)((a, b) => a._1 == a._2 && b)
  }
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this.foldRight(z, Stream(z))((a, b) => {
    lazy val result = f(a, b._1)
    (result, cons(result, b._2))
  })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def main(args: Array[String]): Unit = {
    print(Stream(1,2,3).scanRight(0)(_ + _).toList2)
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

  val ones: Stream[Int] = unfold(empty)((s) => Some((1, s)))
  def constant[A](a: A): Stream[A] = unfold(empty)((s) => Some((a, s)))
  def from(n: Int): Stream[Int] = unfold(n)((s) => Some(s, s + 1))

  val fibs: Stream[Int] = unfold((1, 0))((s) => Some(s._2, (s._1 + s._2, s._1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => empty
  }
}