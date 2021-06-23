package part01_recap

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Try

object AdvancedRecap extends App {
  // Partial functions
  val partialFunction01 = new PartialFunction[Int, Int] {
    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2 || x == 3

    override def apply(v1: Int): Int = v1 match {
      case 1 => 7
      case 2 => 77
      case 3 => 777
    }
  }

  val partialFunction02: PartialFunction[Int, Int] = {
    case 1 => 7
    case 2 => 77
    case 3 => 777
  }

  val partialFunction03 = (x: Int) => x match {
    case 1 => 7
    case 2 => 77
    case 3 => 777
  }

  val partialFunction04: Int => Int = partialFunction03

  val modifiedList = List(1, 2, 3).map {
    case 1 => 73
    case _ => 0
  }

  println(partialFunction02(1))
  println(partialFunction03(2))
  println(partialFunction04(3))

  Try {
    partialFunction01(7)
  }.failed.foreach(fail => println(fail))

  println(modifiedList)

  // Lifting
  val lifted = partialFunction01.lift
  println(lifted(3))
  println(lifted(4))

  // OrElse
  val pfChain = partialFunction02.orElse[Int, Int] {
    case 4 => 42
  }
  println(pfChain(3))
  println(pfChain(4))
  Try {
    pfChain(7)
  }.failed.foreach(fail => println(fail))

  // Type aliases
  type ReceiveFunction = PartialFunction[Any, Unit]

  def receive: ReceiveFunction = {
    case 1 => println("Get the one")
    case _ => println("Confused...")
  }

  receive(1)
  receive(2)

  // Implicits
  implicit val timeout: Int = 3000

  def delayed(f: () => Unit)(implicit timeout: Int): Unit = {
    Thread.sleep(timeout)
    f()
  }

  // Second parameter list omitted
  delayed(() => println("Hello timeouted..."))

  // Implicits conversions
  // 1. Implicit defs
  case class Person(name: String) {
    def greet: String = s"Hello, my name is $name"
  }

  implicit def fromStringToPerson(s: String): Person = Person(s)

  println("Diana".greet)
  // Under the hood in code above compiler call:
  // fromStringToPerson("Diana").greet

  // 2. Implicit classes
  implicit class Dog(name: String) {
    def bark(): Unit = println("Bark!")
  }

  "Lessie".bark()
  // Here's compiler call:
  // new Dog("Lessie").bark()

  // Organize
  // 1. Local scope
  implicit val inverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  println(List(1, 2, 3, 4, 5).sorted)

  // 2. Imported scope
  import scala.concurrent.ExecutionContext.Implicits.global

  val future = Future {
    println("Hello from Future!")
  }

  // 3. Companion objects of the types included in the call
  object Person {
    implicit val personOrdering: Ordering[Person] =
      Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }
  println(List(Person("Diana"), Person("Alice"), Person("Bob")).sorted)
}