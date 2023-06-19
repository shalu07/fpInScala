import scala.math.Numeric.Implicits._
import scala.collection.immutable.List
import scala.collection.immutable.Nil.flatMap

class ex3 extends App{
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object LList {
    def sum(ints: List[Int]) : Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  //exercise number 2: Remove the first element of the list
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t)  => t
    }

  //exercise number 3: Replace the first element with diff value
  def setHead[A](l: List[A], head: A): List[A] = l match {

    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(head, t)
  }

  //exercise number 4: drop first n elements
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) {
      l
    }
    else l match{
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  //exercise number 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t)
        if f(h) => dropWhile(t, f)
      case _ => l
    }
  //exercise number 6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  //exercise number 7
  //Can product, implemented using foldRight,
  // immediately halt the recursion and return 0.0 if it encounters a 0.0? Why or why not?
  // ans - foldRight (or any function) we evaluate it's argument, which means traversing the list
  // all the way to the end.

  //exercise 9 calculate length using foldRight

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  //def length[A](l: List[A]): Int =
    //foldRight(l, 0, (_, acc) => acc + 1)

  //exercise 10 foldLeft tail recursive
  @annotation.tailrec
  private def foldLeft[A, B](l: List[A], z: B, f: (B, A) => B): B =
    l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h), f)
  }

  //exercise 11 sum product and length
  def sumViaFoldLeft(l: List[Int]):Int = foldLeft[Int, Int](l, 0, _ + _)

  def productViaFoldLeft(l: List[Int]):Int = foldLeft[Int, Int](l, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc:Int, _:A) => acc + 1)

  //exercise 12 return reverse of the list
  def reverse[A](l: List[A]): List[A] =
    foldLeft[A, List[A]](l, Nil, (acc, h) => Cons(h, acc))

  //exercise 14 append using foldLeft
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  //exercise 16 transform list by adding 1 to each element
  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])( (h, t) => Cons(h + 1, t))

  //exercise 17 toString using list
  def doubleToString(l: List[Int]): List[String] =
    foldRight(l, Nil: List[String])( (h, t) => Cons(h.toString, t))

  //exercise 18 map implementation
  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  //exercise 19 filter implementation
  def filter[A](l: List[A], f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])( (h, t) =>
      if (f(h))
        Cons(h, t)
      else
        t
    )

  //exercise 20 flatMap Implementation

  def concat[A](l1: List[A], l2: List[A]): List[A] =
    l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(h, concat(t, l2))
    }

  def flatMap[A, B](l: List[A], f: A => List[B]): List[B] =
    concat(map(l, f), Nil: List[A])

  //exercise 21 using flatMap to implement filter

  def filterViaFlatMap[A](l: List[A], f: A => Boolean): List[A] =
    flatMap(l, (a: A) =>
      if (f(a))
        List(a)
      else Nil
    )

}

object ex3_run extends App {
  val ex = new ex3()

  val myList = ex.LList(1, 2, 3, 4, 5)
  val yourList = ex.LList(6,7,8,9)
  val tailList = ex.tail(myList)
  println(tailList)
  println("====================================================")

  val setHeadList = ex.setHead(myList, 9)
  println(setHeadList)
  println("====================================================")

  val dropNList = ex.drop(myList, 2)
  println(dropNList)
  println("====================================================")


  val result = ex.dropWhile(myList, (n: Int) => n < 3)
  println(result)
  println("====================================================")

  val initList = ex.init(myList)
  println(initList)
  println("====================================================")

  val sumViaFoldLeft = ex.sumViaFoldLeft(myList)
  val productViaFoldLeft = ex.productViaFoldLeft(myList)
  val lengthViaFoldLeft = ex.lengthViaFoldLeft(myList)

  println(sumViaFoldLeft)
  println(productViaFoldLeft)
  println(lengthViaFoldLeft)
  println("====================================================")

  val reversList = ex.reverse(myList)
  println(reversList)
  println("====================================================")

  val ourList = ex.appendViaFoldRight(myList, yourList)
  println(ourList)
  println("====================================================")

  val incrementEach = ex.incrementEach(myList)
  println(incrementEach)
  println("====================================================")

  val doubleToString = ex.doubleToString(myList)
  println(doubleToString)
  println("====================================================")

  val map = ex.map(myList, (n: Int) => n + 3)
  println(map)
  val testList = List(1,2,3,4)
  testList.map(x => x + 3)
  println("====================================================")

  val filter = ex.filter(myList, (n: Int) => n < 3)
  println(filter)
  println("====================================================")

  val flatmap = ex.flatMap(myList, (x: Int) => List(x,x))

  val filterbyflatmap = ex.filterViaFlatMap(myList, (x: Int) => x % 2 == 0)


}
