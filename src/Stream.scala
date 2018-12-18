package fpinscala.laziness
import Stream._

trait Stream[+A] {
  // 5.1
  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => {
        //println("1" , acc)
        go(t(), h() :: acc)
      }
      case _ => {
        //println("2" , acc)
        acc
      }
    }
    go(this, List()).reverse
  }

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this // not empty
  }

  // 5.3
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  // 5.4
  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  // 5.5
  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h,t)
      else      empty)

  // 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  // 5.8
  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  // 5.9
  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  // 5.10
  def fibs() : Stream[Int] = {
    def fib(f0: Int, f1 :Int) : Stream[Int] =
      cons(f0, fib(f1, f0 + f1))
    fib(0, 1)
  }

  // 책에서 구현한거 1
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // 책에서 구현한거 2
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)
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
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // Run
  def main(args: Array[String]): Unit = {
    val s = Stream(1,2,3,4,5,6,7,8,9,10)
    // 5.1
    println("5.1", s.toList)

    // 5.2
    println("5.2", s.take(8).toList)
    println("5.2", s.drop(3).toList)

    // 5.3
    println("5.3", s.takeWhile(_ < 4).toList)

    // 5.4
    println("5.4", s.forAll(_ < 20))
    println("5.4", s.forAll(_ > 10))

    // 5.5
    println("5.5", s.takeWhile2(_ < 4).toList)

    // 5.6
    println("5.6", s.drop(4).headOption)

    // 5.7
    println("5.7", s.map(_ + 10).toList)
    println("5.7", s.map(_ + 10).filter(_ > 15).toList)
    println("5.7", s.map(_ + 10).append(Stream(20,30)).toList)

    val a = Stream(Stream(1,2,3), Stream(4,5,6), Stream(7,8,9))
    println("5.7", a.flatMap(_.map(_ + 10)).toList)

    // 5.8
    //println("5.8", s.constant(10).toList)

    // 5.9
    //println("5.9", s.from(1).toList)

    // 5.10
    println("5.10", s.fibs())
  }
}
