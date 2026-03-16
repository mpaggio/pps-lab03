package it.unibo.pps.lab

object LabRun {
  enum Stream[A]:
    case Empty()
    case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def generate[A](generator: () => A): Stream[A] = Cons(generator, () => generate(generator))

    def getHead[A](stream: Stream[A]): A = stream match
      case Cons(h,t) => h()

  @main
  def executeGenerate(): Unit =
    import Stream.*
    val intStream = generate(() => 10)
    println(getHead(intStream))
}
