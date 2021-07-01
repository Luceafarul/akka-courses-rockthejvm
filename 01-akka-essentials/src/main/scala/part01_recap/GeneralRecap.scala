package part01_recap

import scala.annotation.tailrec
import scala.util.Try

object GeneralRecap extends App {
  // values and variables
  val aCondition: Boolean = false
  var aVariable = 41
  aVariable += 1

  // expressions
  val aConditionedValue = if (aCondition) 42 else 73

  // code block
  val aCodeBlock = {
    if (aCondition) 42
    71
  }

  // types
  // Unit
  val theUnit: Unit = println("Hello, Scala!")

  def aFunction(x: Int): Int = x + 1

  // Recursion
  def factorial(x: Int): Int = {
    @tailrec
    def loop(x: Int, acc: Int): Int =
      if (x <= 0) acc
      else loop(x - 1, acc * x)

    loop(x, 1)
  }

  // OOP
  trait Animal

  class Dog extends Animal

  val aDog: Animal = new Dog()

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch-crunch!")
  }

  // Method notation
  val aCrocodile = new Crocodile()
  aCrocodile.eat(aDog)
  aCrocodile eat aDog

  // Anonymous classes
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar!")
  }
  aCarnivore eat aDog

  // Generics
  abstract class MyList[+A]

  // Companion objects
  object MyList

  // Case classes
  case class Person(name: String, age: Int)

  // Exceptions
  val aPotentialFailure = try {
    throw new RuntimeException("Failed, I have type - Nothing")
  } catch {
    case e: Exception =>
      println(s"Exception: ${e.getMessage}")
      "I caught an exception"
  } finally {
    println("Some logs...")
  }

  // Functional programming
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  val incremented = incrementer(41)
  incrementer.apply(41)

  val anonymousIncrementer = (x: Int) => x + 1
  // anonymousIncrementer has type Int => Int that equals to Function1[Int, Int]

  // FP is all about working with functions as first-class
  List(1, 2, 3).map(incrementer)
  // map is High Order Function (HOF)

  // For-comprehension
  val pairs = for {
    num <- List(1, 2, 3, 4, 5)
    char <- List('a', 'b', 'c', 'd', 'e')
  } yield num -> char
  // Code above translate into:
  // List(1, 2, 3, 4, 5).flatMap { num =>
  //   List('a', 'b', 'c', 'd', 'e').map { char =>
  //     num -> char
  //   }
  // }

  // Option and Try
  val anOption = Some(73)
  val aTry = Try {
    throw new RuntimeException("Failed in Try constructions")
  }

  // Pattern matching
  val unknown = 3
  val order = unknown match {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case _ => "unknown"
  }

  val bob = Person("Bob", 21)
  val greeting = bob match {
    case Person(name, _) => s"Hi, my name is $name!"
    case _ => s"I dont know my name! Who I'm?"
  }
}
