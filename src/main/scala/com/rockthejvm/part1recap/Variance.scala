package com.rockthejvm.part1recap

object Variance {

  // OOP - substitution
  class Animal
  class Dog(name: String) extends Animal

  // Variance question for List: if Dog <: Animal, then should List[Dog] <: List[Animal]?

  // YES - COVARIANT
  val lassie = new Dog("Lassie")
  val hachi  = new Dog("Hachi")
  val laika  = new Dog("Laika")

  val anAnimal: Animal          = lassie
  val someAnimals: List[Animal] = List(lassie, hachi, laika)

  class MyList[+A] // MyList is COVARIANT in A
  val myAnimalList: MyList[Animal] = new MyList[Dog]

  // NO - then the type is INVARIANT
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  // all generics in Java
  // val aJavaList: java.util.ArrayList[Animal] = new util.ArrayList[Dog]()

  // HELL NO - CONTRAVARIANCE
  trait Vet[-A] {
    def heal(animal: A): Boolean
  }

  // Vet[Animal] is "better" than a Vet[Dog]
  // Dog <: Animal, then Vet[Dog] >: Vet[Animal]
  val myVet: Vet[Dog] = new Vet[Animal] {
    override def heal(animal: Animal): Boolean = {
      println("Here you go, you're good now...")
      true
    }
  }

  val healingLassie = myVet.heal(lassie)

  /*
   * Rule of thumb:
     - if the type PRODUCES or RETRIEVES values of type A (e.g. lists), then the type should be COVARIANT
     - if the type CONSUMES or ACTS ON values of type A (e.g. a vet), then the type should be CONTRAVARIANT
     - otherwise, INVARIANT
   */

  /**
   * Variance positions
   */

  /* class Cat extends Animal
   *
   * class Vet2[-A](val favoriteAnimal: A) <-- the types of val fields are in COVARIANT position
   *
   * val garfield = new Cat
   * val theVet: Vet2[Animal] = new Vet2[Animal](garfield)
   * val dogVet: Vet2[Dog] = theVet
   * val favAnimal: Dog = dogVet.favoriteAnimal // must be a Dog - type conflict
   */

  // var fields are also in COVARIANT position (same)
  /*
   class MutableContainer[+A](var contents: A) <-- types of vars are in a CONTRAVARIANT position

   val containerAnimal: MutableContainer[Animal] = new MutableContainer[Dog](new Dog)
   containerAnimal.contents = new Cat // type conflict

   So vars are in both covariant and contravariant positions
   */

  // types of method arguments are in CONTRAVARIANT positions
  /*
  class MyList2[+A] {
    def add(element: A): MyList[A]
  }

  val animals: MyList2[Animal] = new MyList2[Cat]
  val biggerAnimals: MyList2[Animal] = animals.add(new Dog) // type conflict!
   */

  // solution: WIDEN the type argument. Adding a cat to a list of dogs, makes it a list of animals now
  class MyList2[+A] {
    def add[B >: A](element: B): MyList[B] = ???
  }

  val l: MyList2[Animal] = new MyList2[Dog] {
    override def add[B >: Dog](element: B): MyList[B] = ???
  }

  // method return types are in COVARIANT position
  /*
  abstract class Vet2[-A] {
    def rescueAnimal(): A
  }

  val vet: Vet2[Animal] = new Vet2[Animal] {
    def rescueAnimal(): Animal = new Cat
  }
  val lassieVet: Vet2[Dog] = vet
  val rescueDog: Dog = lassieVet.rescueAnimal() // must return a Dog, but it returns a Cat - type conflict!
   */

  class Cat(name: String) extends Animal

  abstract class Vet2[-A] {
    def rescueAnimal[B <: A](): B
  }
  val vet: Vet2[Dog] = new Vet2[Animal] {
    override def rescueAnimal[B <: Animal](): B = ???
  }

  val lassieVet: Vet2[Dog] = vet
  val rescueCat: Cat       = lassieVet.rescueAnimal()

  def main(args: Array[String]): Unit = {}
}
