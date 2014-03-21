// Examples from:
//
// [YJJGKO]
// Yakushev, Jeuring, Jansson, Gerdes, Kiselyov, Oliveira.
// Comparing datatype generic libraries in Haskell.

trait DataTypes
extends Companies
   with BinTrees

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
  case class Bin[A](leftChild: BinTree[A], rightChild: BinTree[A]) extends BinTree[A]

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
