package com.jsuereth.collections

import scala.collection.GenTraversableLike

trait MyIsTraversableLike[Repr] {
  /** The type of elements we can traverse over. */
  type A
  /** A conversion from the representation type `Repr` to a `GenTraversableLike[A,Repr]`. */
  val conversion: Repr => GenTraversableLike[A, Repr]
}

object MyIsTraversableLike {
  import scala.language.higherKinds

  implicit val stringRepr: MyIsTraversableLike[String] { type A = Char } = {
    @inline
    class StringIsTraversableLike extends MyIsTraversableLike[String] {
      type A = Char
      val conversion = implicitly[String => GenTraversableLike[Char, String]]
    }
    new StringIsTraversableLike
  }

  implicit def genTraversableLikeRepr[C[_], A0](implicit conv: C[A0] => GenTraversableLike[A0,C[A0]]): MyIsTraversableLike[C[A0]] { type A = A0 } = {
    @inline
    class IsTraversableLikeThroughConv extends MyIsTraversableLike[C[A0]] {
      type A = A0
      val conversion = conv
    }
    new IsTraversableLikeThroughConv
  }
}
