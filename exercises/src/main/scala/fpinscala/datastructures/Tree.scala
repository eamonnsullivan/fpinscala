package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A,B](t: Tree[A])(lf: A => B)(bf: (B,B) => B): B = t match {
      case Leaf(v) => lf(v)
      case Branch(l,r) => bf(fold(l)(lf)(bf),fold(r)(lf)(bf))
    }

  def size[A](t: Tree[A]): Int = fold(t)(x => 1)(1 + _ + _)

  def maximum(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  def depth[A](t: Tree[A]): Int = fold(t)(x => 1)(_ + 1 max _ + 1)

  def map[A, B](l: Tree[A])(f: A => B): Tree[B] = fold(l)(x => Leaf(f(x)): Tree[B])(Branch(_,_))
}
