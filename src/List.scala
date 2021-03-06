sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  // 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case nil => nil
      case Cons(_, t) => t
    }
  }
  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case nil => nil
      case Cons(_, t) => Cons(h, t)
    }
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
      l match {
        case nil => nil
        case Cons(_, t) => drop(t, n-1)
      }
    }

    // 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      l match {
        case nil => nil
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => l
      }
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    // 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

    def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](l: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
