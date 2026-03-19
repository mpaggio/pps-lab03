package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*

class OptionTest:

  // imports needed to work with Options
  import u03.Optionals.*
  import Optional.*

  // values under test
  val optSome = Just(5)
  val optNone = Empty()

  @Test def testEmpty(): Unit =
    assertFalse(isEmpty(optSome))
    assertTrue(isEmpty(optNone))

  @Test def testOrElse(): Unit =
    assertEquals(5, orElse(optSome, 10))
    assertEquals(10, orElse(optNone, 10))

  @Test def testMap(): Unit =
    assertEquals(Just(6), map(optSome)(_ + 1))
    assertEquals(Just("val: 5"), map(optSome)(v => "val: " + v))
    assertEquals(Empty(), map(optNone)(v => "val: " + v))
