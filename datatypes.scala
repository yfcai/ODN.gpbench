// Examples from:
//
// [YJJGKO]
// Yakushev, Jeuring, Jansson, Gerdes, Kiselyov, Oliveira.
// Comparing datatype generic libraries in Haskell.

import language.higherKinds
import collection.breakOut

trait DataTypes
extends Companies
   with BinTrees
   with WTrees
   with Roses
   with PerfectTrees

// generate a data type from a number
trait Generator {
}

// paradise benchmark in "scrap your boilerplate"
trait Companies {
  case class Company(departments: List[Department])
  case class Department(name: Name, manager: Manager, dUnits: List[DUnit])
  trait DUnit
  case class PU(employee: Employee) extends DUnit
  case class DU(department: Department) extends DUnit
  case class Employee(person: Person, salary: Salary)
  case class Person(name: Name, address: Address)
  case class Salary(amount: Float)
  type Manager = Employee
  type Name = String
  type Address = String

  val ralf   = Employee(Person("Ralf",   "Amsterdam"), Salary(  8000))
  val joost  = Employee(Person("Joost",  "Amsterdam"), Salary(  1000))
  val marlow = Employee(Person("Marlow", "Cambridge"), Salary(  2000))
  val blair  = Employee(Person("Blair",  "London"   ), Salary(100000))

  // facepalm: if genCom is declared before ralf, jost, marlow & blair then
  // expect NullPointerException.
  val genCom: Company =
    Company(List(
      Department("Research", ralf, List(PU(joost), PU(marlow))),
      Department("Strategy", blair, Nil)))
}

// binary trees with arbitrary uniform data at leaves
trait BinTrees {
  trait BinTree[A]
  case class Leaf[A](get: A) extends BinTree[A]
  case class Bin[A](left: BinTree[A], right: BinTree[A]) extends BinTree[A]

  // the complete binary tree with n leaves labelled 1 to n
  def completeBinTree(n: Int): BinTree[Int] = {
    def log2(n: Int): Int = 31 - java.lang.Integer.numberOfLeadingZeros(n)

    // generate complete binary tree with leaves labelled from to (to - 1)
    def loop(from: Int, to: Int): BinTree[Int] =
      if (to - from == 1)
        Leaf(from)
      else {
        val n = to - from
        val mostSignificantBit = log2(n)
        val pow = 1 << mostSignificantBit
        val halfPow = pow / 2
        //val right = pow / 2 // the largest power of 2 not exceeding n/2
        val mid =
          // case rhs is power of 2
          if (0 == (n & halfPow)) to - halfPow
          // case lhs is power of 2
          else
            from + pow
        Bin(loop(from, mid), loop(mid, to))
      }

    loop(1, n + 1)
  }
}

trait WTrees {
  trait WTree[A, W]
  case class WLeaf[A, W](get: A) extends WTree[A, W]
  case class WBin[A, W](left: WTree[A, W], right: WTree[A, W]) extends WTree[A, W]
  case class WithWeight[A, W](tree: WTree[A, W], weight: W) extends WTree[A, W]

  val genWTree: WTree[Int, Int] =
    WBin(
      WithWeight(WLeaf(17), 1),
      WBin(WLeaf(23), WithWeight(WLeaf(38), 2)))
}

trait Roses {
  // ordinary rose tree
  case class Rose[A](get: A, children: List[Rose[A]])

  // generalized rose tree: no macro support yet
  case class GRose[F[_], A](get: A, children: F[GRose[F, A]])

  // nested generalized rose tree: no macro support yet
  case class NGRose[F[_], A](get: A, children: F[NGRose[Comp[F, F]#λ, A]])
  type Comp[F[_], G[_]] = { type λ[A] = F[G[A]] }

  // http://www.royal.gov.uk/HMTheQueen/Marriageandfamily/Marriageandfamily.aspx
  //
  // The Queen and The Duke of Edinburgh celebrated their 64th wedding
  // anniversary on 20 November 2011.
  //
  // They have four children, eight grandchildren and three great-grandchildren.

  val elizabeth2: Rose[String] =
    Rose("Elizabeth", List(
      Rose("Charles", List(
        Rose("William", List(
          Rose("George", Nil))),
        Rose("Henry", Nil))),
      Rose("Anne", List(
        Rose("Peter Phillips", List(
          Rose("Savannah", Nil),
          Rose("Isla", Nil))),
        Rose("Zara Phillips", Nil))),
      Rose("Andrew", List(
        Rose("Beatrice", Nil),
        Rose("Eugenie", Nil))),
      Rose("Edward", List(
        Rose("Louise", Nil),
        Rose("Severn", Nil)))))

  val elizabeth2G: GRose[Set, String] = {
    def loop(rose: Rose[String]): GRose[Set, String] = rose match {
      case Rose(name, children) => GRose(name, children.map(loop)(breakOut))
    }
    loop(elizabeth2)
  }

  type List2[A] = List[List[A]]
  type List4[A] = List2[List2[A]]
  type List8[A] = List4[List4[A]]

  val elizabeth2NG: NGRose[List, String] =
    NGRose[List, String]("Elizabeth", List(
      NGRose[List2, String]("Charles", List(List(
        NGRose[List4, String]("William", List(List(List(List(
          NGRose[List8, String]("George", Nil)
        ))))),
        NGRose[List4, String]("Henry", Nil)
      )))
      // other princes & princesses omitted due to tedium
    ))
}

// perfect binary trees
trait PerfectTrees {
  trait Perfect[A]
  case class Zero[A](get: A) extends Perfect[A]
  case class Succ[A](get: Perfect[Fork[A]]) extends Perfect[A]
  case class Fork[A](left: A, right: A) // is not a perfect binary tree

  // create perfect binary tree with leaves from 1 to 2^n
  def perfectBinTree(n: Int): Perfect[_] = {
    def loop(n: Int, from: Int): Perfect[_] =
      if (n == 0)
        Zero(from)
      else
        Succ(Zero(Fork(loop(n - 1, from), loop(n - 1, from + (1 << (n - 1))))))
    loop(n, 1)
  }

  def pbin[A](left: Perfect[A], right: Perfect[A]): Perfect[Perfect[A]] =
    Succ(Zero(Fork(left, right)))
}
