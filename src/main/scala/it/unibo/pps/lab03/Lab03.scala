package it.unibo.pps.lab03

object Lab03 extends App {
  import u03.Sequences.Sequence
  import u03.Sequences.Sequence.*
  import it.unibo.pps.u02.AlgebraicDataTypes.Person
  import it.unibo.pps.u02.AlgebraicDataTypes.Person.*
  import u03.Optionals.Optional

  object Task1:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    @annotation.tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
      case (Cons(h, t), 0) => Cons(h, t)
      case (Cons(h, t), n) => skip(t)(n - 1)
      case (_, _) => Nil()

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case (_, _) => Nil()

    def zipTailRec[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      @annotation.tailrec
      def zipRec(first: Sequence[A], second: Sequence[B], acc: Sequence[(A, B)]): Sequence[(A, B)] = (first, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => zipRec(t1, t2, Cons((h1, h2), acc))
        case (_, _) => acc
      reverse(zipRec(first, second, Nil()))

    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Cons(h1, t1), _) => Cons(h1, concat(t1, s2))
      case (_, Cons(h2, t2)) => Cons(h2, t2)
      case (_, _) => Nil()

    def concatTailRec[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def concatRec(s1: Sequence[A], s2: Sequence[A], acc: Sequence[A]): Sequence[A] = (s1, s2) match
        case (Cons(h1, t1), _) => concatRec(t1, s2, Cons(h1, acc))
        case (_, Cons(h2, t2)) => concatRec(t2, Nil(), Cons(h2, acc))
        case (_, _) => acc
      reverse(concatRec(s1, s2, Nil()))

    def reverse[A](s: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def rev(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Cons(h, t) => rev(t, Cons(h, acc))
        case Nil() => acc
      rev(s, Nil())

    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    def flatMapTailRec[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      @annotation.tailrec
      def flatMapRec(s: Sequence[A])(acc: Sequence[B]): Sequence[B] = s match
        case Cons(h, t) => flatMapRec(t)(concatTailRec(acc, mapper(h)))
        case _ => acc
      flatMapRec(s)(Nil())

    def min(s: Sequence[Int]): Optional[Int] =
      @annotation.tailrec
      def minRec(s: Sequence[Int], min: Optional[Int]): Optional[Int] = (s, min) match
        case (Cons(h, t), Optional.Just(a)) if a < h => minRec(t, Optional.Just(a))
        case (Cons(h, t), _) => minRec(t, Optional.Just(h))
        case _ => min
      minRec(s, Optional.Empty())

    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def even(s: Sequence[A], acc: Sequence[A], counter: Int): Sequence[A] = s match
        case Cons(h, t) if counter % 2 == 0 => even(t, concatTailRec(acc, Cons(h, Nil())), counter + 1)
        case Cons(_, t) => even(t, acc, counter + 1)
        case _ => acc
      even(s, Nil(), 0)

    @annotation.tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Nil() => false
      case Cons(h, t) if h != elem => contains(t)(elem)
      case _ => true

    def distinct[A](s: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def remove(s: Sequence[A], found: Sequence[A]): Sequence[A] = s match
        case Cons(h, t) if !contains(found)(h) => remove(t, concatTailRec(found, Cons(h, Nil())))
        case Cons(_, t) => remove(t, found)
        case _ => found
      remove(s, Nil())

    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
      @annotation.tailrec
      def search(s: Sequence[A], last: Sequence[A], acc: Sequence[Sequence[A]]): Sequence[Sequence[A]] = (s, last) match
        case (Nil(), Nil()) => Nil()
        case (Cons(head, tail), Nil()) => search(tail, Cons(head, Nil()), acc)
        case (Cons(head, tail), Cons(lastHead, _)) if head == lastHead =>
          search(tail, concatTailRec(last, Cons(head, Nil())), acc)
        case (Cons(head, tail), _) => search(tail, Cons(head, Nil()), Cons(last, acc))
        case (_, _) => reverse(Cons(last, acc))
      search(s, Nil(), Nil())

    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @annotation.tailrec
      def part(s: Sequence[A], first: Sequence[A], second: Sequence[A]): (Sequence[A], Sequence[A]) = s match
        case Cons(h, t) if pred(h) => part(t, Cons(h, first), second)
        case Cons(h, t) => part(t, first, Cons(h, second))
        case _ => (reverse(first), reverse(second))
      part(s, Nil(), Nil())

  object Task2:

    def getCourses1(persons: Sequence[Person]): Sequence[String] =
      Sequence.map(Sequence.filter(persons)(p => p match
          case Teacher(_,_) => true
          case _ => false
        ))(p => p match
          case Teacher(_, c) => c
        )

    def getCourses2(persons: Sequence[Person]): Sequence[String] =
      flatMap(persons)(p => p match
        case Teacher(_, c) => Cons(c,Nil())
        case _ => Nil()
      )

    def foldLeft[A,B](s: Sequence[A])(default: B)(operator: (B,A) => B): B =
      @annotation.tailrec
      def fold(s: Sequence[A], acc: B): B = s match
        case Cons(h,t) => fold(t, operator(acc, h))
        case Nil() => acc
      fold(s, default)

    def getDistinctCourses(s: Sequence[Person]): Int =
      foldLeft(distinct(getCourses1(s)))(0)((acc, _) => acc + 1)

  object Task3:

    enum Stream[A]:
      private case Empty()
      private case Cons(head: () => A, tail: () => Stream[A])

    object Stream:

      def empty[A](): Stream[A] = Empty()

      def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)

      def toList[A](stream: Stream[A]): Sequence[A] = stream match
        case Cons(h, t) => Sequence.Cons(h(), toList(t()))
        case _ => Sequence.Nil()

      def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
        case Cons(head, tail) => cons(f(head()), map(tail())(f))
        case _ => Empty()

      def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
        case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
        case Cons(head, tail) => filter(tail())(pred)
        case _ => Empty()

      def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
        case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
        case _ => Empty()

      def iterate[A](init: => A)(next: A => A): Stream[A] =
        cons(init, iterate(next(init))(next))

      def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
        case Cons(h,t) if pred(h()) => cons(h(), takeWhile(t())(pred))
        case _ => Empty()

      def fill[A](n: Int)(k: A): Stream[A] =
        take(iterate(k)(_ => k))(n)

      def fibonacci(): Stream[Int] =
        def fib(first: => Int, second: => Int): Stream[Int] =
          cons(first, fib(second, first + second))
        fib(0,1)


}
