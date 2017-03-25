package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new Exception("Tail of empty list")
      case Cons(h, t) => t
    }

  def head[A](l: List[A]): A =
    l match {
      case Nil => throw new Exception("Head of empty list")
      case Cons(h,_) => h
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (n <= 0) {
          l
        } else {
          drop(t, n - 1)
        }
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new Exception("init on empty list")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, x) => x + 1)
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(lst: List[A], acc: B): B =
      lst match {
        case Nil => acc
        case Cons(x, xs) => loop(xs, f(acc, x))
      }
    loop(l, z)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def reverse[A](ns: List[A]): List[A] = foldLeft(ns, Nil: List[A])((l, h) => Cons(h, l))

  def append2[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])(append2(_, _))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def go(lst: List[A], acc: List[A]): List[A] = {
      lst match {
        case Cons(h, t) =>
          if (f(h)) {
            go(t, append(acc, List[A](h)))
          } else {
            go(t, acc)
          }
        case _ => acc
      }
    }
    go(as, Nil: List[A])
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def zipWith_first_attempt[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] = {
    def go(a: List[A], b: List[A], acc: List[A]): List[A] = {
      a match {
        case Cons(h,t) => b match {
          case Cons(hb, tb) =>
            go(t, tb, append(acc, List(f(h,hb))))
          case Nil => append(acc, a)
        }
        case Nil => append(acc, b)
      }
    }
    go(l,r, Nil:List[A])
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = (a, b) match {
      case (_,Nil) => Nil
      case (Nil,_) => Nil
      case (Cons(h1,t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith[A](t1,t2)(f))
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => (sub == Nil)
      case Cons(h,t) =>
        if (sub == Nil) {
          true
        } else if (head(sub) == h) {
          hasSubsequence(t, tail(sub))
        } else {
          hasSubsequence(t, sub)
        }
    }

}
