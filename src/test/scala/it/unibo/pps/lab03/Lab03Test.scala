package it.unibo.pps.lab03

import org.junit.Assert.assertEquals
import org.junit.Test

object Lab03Test:
  import u03.Sequences.Sequence
  import u03.Sequences.Sequence.*
  import it.unibo.pps.u02.AlgebraicDataTypes.Person
  import it.unibo.pps.u02.AlgebraicDataTypes.Person.*
  import it.unibo.pps.lab03.Lab03.Task2.*
  import it.unibo.pps.lab03.Lab03.Task3.Stream
  import it.unibo.pps.lab03.Lab03.Task3.Stream.*
  import u03.Optionals.Optional.*

  class Task1Test:

    import Sequence.*

    val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

    @Test def testSum(): Unit =
      assertEquals(0, sum(Nil()))
      assertEquals(60, sum(sequence))

    @Test def testMap(): Unit =
      assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(sequence)(_ + 1))
      assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(sequence)(_ + ""))

    @Test def testFilter(): Unit =
      assertEquals(Cons(20, Cons(30, Nil())), filter(sequence)(_ >= 20))
      assertEquals(Cons(10, Cons(30, Nil())), filter(sequence)(_ != 20))

    @Test def testSkip(): Unit =
      assertEquals(Cons(30, Nil()), skip(sequence)(2))
      assertEquals(Nil(), skip(sequence)(3))
      assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
      assertEquals(Nil(), skip(Nil())(2))

    @Test def testZip(): Unit =
      val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
      assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
      assertEquals(Nil(), zip(sequence, Nil()))
      assertEquals(Nil(), zip(Nil(), l2))
      assertEquals(Nil(), zip(Nil(), Nil()))

    @Test def testZipTailRec(): Unit =
      val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
      assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zipTailRec(sequence, l2))
      assertEquals(Nil(), zipTailRec(sequence, Nil()))
      assertEquals(Nil(), zipTailRec(Nil(), l2))
      assertEquals(Nil(), zipTailRec(Nil(), Nil()))

    @Test def testConcat(): Unit =
      val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
      assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
      assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

    @Test def testConcatTailRec(): Unit =
      val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
      assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concatTailRec(sequence, l2))
      assertEquals(Cons(40, Cons(50, Nil())), concatTailRec(Nil(), l2))

    @Test def testReverse(): Unit =
      assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
      assertEquals(Nil(), reverse(Nil()))

    @Test def testFlatMap(): Unit =
      assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
      assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

    @Test def testFlatMapTailRec(): Unit =
      assertEquals(Cons(11, Cons(10, Cons(21, Cons(20, Cons(31, Cons(30, Nil())))))), flatMapTailRec(sequence)(v => Cons(v + 1, Cons(v, Nil()))))
      assertEquals(Nil(), flatMapTailRec(Nil())(v => Cons(v, Nil())))

    @Test def testMin(): Unit =
      assertEquals(Just(10), min(sequence))
      assertEquals(Just(1), min(Cons(1, Nil())))
      assertEquals(Empty(), min(Nil()))

    @Test def testEvenIndices(): Unit =
      assertEquals(Cons(10, Cons(30, Nil())), evenIndices(sequence))
      assertEquals(Nil(), evenIndices(Nil()))

    @Test def testContains(): Unit =
      assertEquals(true, contains(sequence)(10))
      assertEquals(false, contains(sequence)(15))
      assertEquals(false, contains(Nil())(10))

    @Test def testDistinct(): Unit =
      assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), distinct(sequence))
      assertEquals(Cons(10, Cons(20, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
      assertEquals(Nil(), distinct(Nil()))

    @Test def testGroup(): Unit =
      val sequence = Cons(10, Cons(10, Cons(20, Cons(30, Cons(20, Nil())))))
      val grouped =
        Cons(Cons(10, Cons(10, Nil())), Cons(Cons(20, Nil()), Cons(Cons(30, Nil()), Cons(Cons(20, Nil()), Nil()))))
      assertEquals(group(sequence), grouped)
      assertEquals(Nil(), group(Nil()))

    @Test def testPartition(): Unit =
      val sequence = Cons(11, Cons(20, Cons(31, Nil())))
      val (even, odd) = partition(sequence)(x => x % 2 == 0)
      assertEquals(Cons(20, Nil()), even)
      assertEquals(Cons(11, Cons(31, Nil())), odd)

      val emptySequence = Nil()
      val (evenEmpty, oddEmpty) = partition(emptySequence)(x => true)
      assertEquals(Nil(), evenEmpty)
      assertEquals(Nil(), oddEmpty)

  class Task2Test:

    @Test
    def testGetCoursesWithFullSequence(): Unit =
      val persons: Sequence[Person] = Cons(Student("Paggetti", 2003), Cons(Teacher("Aguzzi", "PPS"), Cons(Teacher("Viroli", "PPS"), Nil())))
      assertEquals(Cons("PPS", Cons("PPS", Nil())), getCourses1(persons))
      assertEquals(getCourses1(persons),getCourses2(persons))

    @Test
    def testGetCoursesWithEmptySequence(): Unit =
      val persons: Sequence[Person] = Nil()
      assertEquals(Nil(), getCourses1(persons))
      assertEquals(getCourses1(persons), getCourses2(persons))

    @Test
    def testGetCoursesWithNoTeacher(): Unit =
      val persons: Sequence[Person] = Cons(Student("Paggetti", 2003), Cons(Student("Rossi", 2003), Nil()))
      assertEquals(Nil(), getCourses1(persons))
      assertEquals(getCourses1(persons), getCourses2(persons))

    @Test
    def testFoldLeftWithNonEmptySequence(): Unit =
      val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
      assertEquals(-16, foldLeft(lst)(0)(_ - _))

    @Test
    def testFoldLeftWithEmptySequence(): Unit =
      val lst: Sequence[Int] = Nil()
      val default: Int = 0
      assertEquals(default, foldLeft(lst)(default)(_ + _))

    @Test
    def testGetDistinctCoursesWithNonEmptySequence(): Unit =
      val sequence: Sequence[Person] = Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Aguzzi", "PPS"), Cons(Teacher("Ricci", "PCD"), Nil())))
      assertEquals(2, getDistinctCourses(sequence))

    @Test
    def testGetDistinctCoursesWithEmptySequence(): Unit =
      val sequence: Sequence[Person] = Nil()
      assertEquals(0, getDistinctCourses(sequence))

    @Test
    def testGetDistinctCoursesWithNoTeachers(): Unit =
      val sequence: Sequence[Person] = Cons(Student("Paggetti", 2003), Nil())
      assertEquals(0, getDistinctCourses(sequence))

  class Task3Test:

    @Test
    def testTakeWhile(): Unit =
      val stream = Stream.iterate(0)(_ + 1)
      assertEquals(Cons (0 , Cons (1 , Cons (2 , Cons (3 , Cons (4 , Nil ()))))), Stream.toList(Stream.takeWhile(stream)(_ < 5)))

    @Test
    def testTakeWhileWithEmptyStream(): Unit =
      val stream: Stream[Int] = empty()
      assertEquals(Nil(), Stream.toList(Stream.takeWhile(stream)(_ < 5)))

    @Test
    def testTakeWhileWithNoSuccessfulElement(): Unit =
      val stream = Stream.iterate(0)(_ + 1)
      assertEquals(Nil(), Stream.toList(Stream.takeWhile(stream)(_ < -1)))

    @Test
    def testFill(): Unit =
      assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(Stream.fill(3)("a")))

    @Test
    def testFillWithNothing(): Unit =
      assertEquals(Nil(), Stream.toList(Stream.fill(0)("a")))

    @Test
    def testFibonacci(): Unit =
      assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), Stream.toList(Stream.take(fibonacci())(5)))