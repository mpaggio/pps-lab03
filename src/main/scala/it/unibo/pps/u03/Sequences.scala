package u03

import u03.Optionals.Optional

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @annotation.tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s,n) match
      case (Cons(h,t), 0) => Cons(h,t)
      case (Cons(h,t), n) => skip(t)(n - 1)
      case (_, _) => Nil()

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case (_, _) => Nil()

    def zipTailRec[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      @annotation.tailrec
      def zipRec(first: Sequence[A], second: Sequence[B], acc: Sequence[(A, B)]): Sequence[(A, B)] = (first, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => zipRec(t1, t2, Cons((h1, h2), acc))
        case (_,_) => acc
      reverse(zipRec(first, second, Nil()))

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1,s2) match
      case (Cons(h1,t1), _) => Cons(h1, concat(t1, s2))
      case (_, Cons(h2,t2)) => Cons(h2,t2)
      case (_, _ ) => Nil()

    def concatTailRec[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def concatRec(s1: Sequence[A], s2: Sequence[A], acc: Sequence[A]): Sequence[A] = (s1,s2) match
        case (Cons(h1, t1), _) => concatRec(t1, s2, Cons(h1, acc))
        case (_, Cons(h2, t2)) => concatRec(t2, Nil(), Cons(h2, acc))
        case (_, _) => acc
      reverse(concatRec(s1,s2,Nil()))

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def rev(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Cons(h,t) => rev(t, Cons(h, acc))
        case Nil() => acc
      rev(s, Nil())

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h,t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    def flatMapTailRec[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      @annotation.tailrec
      def flatMapRec(s: Sequence[A])(acc: Sequence[B]): Sequence[B] = s match
        case Cons(h,t) => flatMapRec(t)(concatTailRec(mapper(h),acc))
        case _ => acc
      reverse(flatMapRec(s)(Nil()))

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] = ???

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = ???

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    def contains[A](s: Sequence[A])(elem: A): Boolean = ???

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] = ???

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = ???

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = ???

@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
