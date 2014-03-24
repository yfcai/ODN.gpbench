// Examples from:
//
// [YJJGKO]
// Yakushev, Jeuring, Jansson, Gerdes, Kiselyov, Oliveira.
// Comparing datatype generic libraries in Haskell.

import language.implicitConversions
import language.existentials
import language.higherKinds
import reflect.ClassTag
import collection.breakOut
import org.scalatest._

// this example is for correctness.
// @converMe macro doesn't make it easier to write
// due to not supporting mutually recursive data types.
trait Operations extends DataTypes {
  // collects all salary nodes in a list
  def selectSalary(company: Company): List[Salary] = {
    def selectSalary(department: Department): List[Salary] =
      department.manager.salary :: (
        for {
          unit <- department.dUnits
          salary = unit match {
            case PU(employee) => List(employee.salary)
            case DU(department) => selectSalary(department)
          }
          _ <- NoReturn
        }
        yield salary
      )
    company.departments flatMap selectSalary
  }

  // increase all salaries by 10%
  def updateSalary(company: Company): Company = {
    def updateEmployee(employee: Employee): Employee =
      employee.copy(salary = Salary(employee.salary.amount * 1.1F))
    def updateDepartment(department: Department): Department =
      department.copy(
        manager = updateEmployee(department.manager),
        dUnits = department.dUnits.map {
          case PU(employee) => PU(updateEmployee(employee))
          case DU(department) => DU(updateDepartment(department))
        })
    company.copy(departments = company.departments map updateDepartment)
  }

  // remove all `WithWeight` nodes
  def rmWeights[A, W, V](wtree: WTree[A, W]): WTree[A, V] = wtree match {
    case WLeaf(x) => WLeaf(x)
    case WBin(left, right) => WBin(rmWeights(left), rmWeights(right))
    case WithWeight(tree, _) => rmWeights(tree)
  }

  // flatten all kinds of data structures into list
  // (here: lists of lists)
  def flatten[A](list: List[_]): List[A] = list match {
    case Nil => Nil
    case (x: List[_]) :: rest => flatten(x) ++ flatten(rest)
    case x :: rest => x.asInstanceOf[A] :: flatten(rest)
  }

  def flatten[A](tree: BinTree[A]): List[A] = tree match {
    case Leaf(x) => List(x)
    case Bin(left, right) => flatten(left) ++ flatten(right)
  }

  def flatten[A, W](tree: WTree[A, W]): List[A] = tree match {
    case WLeaf(x) => List(x)
    case WBin(left, right) => flatten(left) ++ flatten(right)
    case WithWeight(tree, _) => flatten(tree)
  }

  def flatten[A](rose: Rose[A]): List[A] = rose match {
    case Rose(x, children) => x :: children.flatMap(x => flatten(x))
  }

  def flatten[F[X] <: Iterable[X], A](rose: GRose[F, A]): List[A] = rose match {
    case GRose(x, children) => x ::
      (children.flatMap(x => flatten[F, A](x))(breakOut) : List[A])
  }

  def flatten[F[_], A](rose: NGRose[F, A]): List[A] = rose match {
    case NGRose(x, children: List[_]) =>
      x :: (flatten[NGRose[Comp[F, F]#λ, A]](children).flatMap(
        y => flatten[Comp[F, F]#λ, A](y.asInstanceOf[NGRose[Comp[F, F]#λ, A]])))
  }

  def flatten[A](tree: Perfect[_]): List[A] = tree match {
    case Zero(x) => List(x.asInstanceOf[A])
    case Succ(Zero(Fork(lhs, rhs))) =>
      flatten[A](lhs.asInstanceOf[Perfect[_]]) ++
      flatten[A](rhs.asInstanceOf[Perfect[_]])
  }

  def childrenOfQueen(elizabeth: Rose[String]): List[String] =
    elizabeth.children.map(_.get)

  def childrenOfQueen(elizabeth: GRose[Set, String]): List[String] =
    elizabeth.children.map(_.get)(breakOut)

  def childrenOfQueen[F[_]](elizabeth: NGRose[F, String]): List[String] =
    elizabeth.children match {
      case children: List[_] =>
        flatten[NGRose[Comp[F, F]#λ, String]](children).map(_.get)
    }

  def grandChildrenOfQueen(elizabeth: Rose[String]): List[String] =
    elizabeth.children.flatMap(childrenOfQueen)

  def grandChildrenOfQueen(elizabeth: GRose[Set, String]): List[String] =
    elizabeth.children.flatMap(childrenOfQueen)(breakOut)

  def grandChildrenOfQueen(elizabeth: NGRose[List, String]): List[String] =
    elizabeth.children.flatMap(x => childrenOfQueen[List2](x))

  def greatGrandChildrenOfQueen(elizabeth: Rose[String]): List[String] =
    elizabeth.children.flatMap(grandChildrenOfQueen)

  def greatGrandChildrenOfQueen(elizabeth: GRose[Set, String]): List[String] =
    elizabeth.children.flatMap(grandChildrenOfQueen)(breakOut)

  def greatGrandChildrenOfQueen(elizabeth: NGRose[List, String]): List[String] =
    for {
      child <- elizabeth.children
      grandchildren <- flatten[NGRose[List4, String]](child.children)
      greatGrandChildren <- flatten[NGRose[List8, String]](grandchildren.children)
    } yield greatGrandChildren.get
}

class OperationTests extends FlatSpec with Operations {
  "selectSalary" should "collect salary nodes in some order" in {
    val sortedSalaries = selectSalary(genCom).sortBy(_.amount)
    assert(sortedSalaries == List[Float](1000, 2000, 8000, 100000).map(Salary.apply))
  }

  "updateSalary" should "increase salaries by 10%" in {
    val updatedSalaries = selectSalary(updateSalary(genCom)).sortBy(_.amount)
    assert(updatedSalaries == List[Float](1100, 2200, 8800, 110000).map(Salary.apply))
  }

  // sanity test for binary tree generator
  "completeBinTree" should "produce complete binary trees" in {
    val List(bin1, bin2, bin3, bin4, bin5, bin6, bin7, bin8, bin9) =
      (1 to 9).map(completeBinTree).toList
    implicit def convert(n: Int): BinTree[Int] = Leaf(n)
    assert(bin1 == Leaf(1))
    assert(bin2 == Bin(1, 2))
    assert(bin3 == Bin(Bin(1, 2), 3))
    assert(bin4 == Bin(Bin(1, 2), Bin(3, 4)))
    assert(bin5 == Bin(Bin(Bin(1, 2), 3), Bin(4, 5)))
    assert(bin6 == Bin(Bin(Bin(1, 2), Bin(3, 4)), Bin(5, 6)))
    assert(bin7 == Bin(Bin(Bin(1, 2), Bin(3, 4)), Bin(Bin(5, 6), 7)))
    assert(bin8 == Bin(Bin(Bin(1, 2), Bin(3, 4)), Bin(Bin(5, 6), Bin(7, 8))))
    assert(bin9 == Bin(Bin(Bin(Bin(1, 2), 3), Bin(4, 5)), Bin(Bin(6, 7), Bin(8, 9))))
  }

  // remove weights from a binary tree
  "rmWeights" should "remove all WithWeight-nodes" in {
    assert(
      rmWeights(genWTree) ==
        WBin[Int, Nothing](WLeaf(17), WBin[Int, Nothing](WLeaf(23), WLeaf(38))))
  }

  // sanity test for perfect binary tree
  "perfectBinTree" should "produce perfect binary trees" in {
    val List(bin1, bin2, bin4, bin8, bin16) = (0 to 4).map(perfectBinTree).toList
    implicit def convert(n: Int): Perfect[Int] = Zero(n)
    assert(bin1 == Zero(1))
    assert(bin2 == pbin(1, 2))
    assert(bin4 == pbin(pbin(1, 2), pbin(3, 4)))
    assert(bin8 == pbin(pbin(pbin(1, 2), pbin(3, 4)), pbin(pbin(5, 6), pbin(7, 8))))
    assert(bin16 == pbin(
      pbin(pbin(pbin(1, 2), pbin(3, 4)), pbin(pbin(5, 6), pbin(7, 8))),
      pbin(pbin(pbin(9, 10), pbin(11, 12)), pbin(pbin(13, 14), pbin(15, 16)))))
  }

  "flatten" should "work on lists and trees" in {
    assert(flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
    assert(flatten(completeBinTree(5)) == List(1, 2, 3, 4, 5))
    assert(flatten(genWTree) == List(17, 23, 38))

    val royalties = List(
      "Elizabeth", "Charles", "William", "George",
      "Henry", "Anne", "Peter Phillips", "Savannah",
      "Isla", "Zara Phillips", "Andrew", "Beatrice",
      "Eugenie", "Edward", "Louise", "Severn")

    assert(flatten(elizabeth2) == royalties)
    assert(flatten(elizabeth2G) == royalties)
    assert(flatten(elizabeth2NG) == royalties.take(5)) // only listed 5 royalties there

    assert(flatten(perfectBinTree(4)) == (1 to 16).toList)
  }

  "Her Majesty the Queen" should "have four children" in {
    assert(childrenOfQueen(elizabeth2).length == 4)
    assert(childrenOfQueen(elizabeth2G).length == 4)

    // did not list all childrens
    assert(childrenOfQueen(elizabeth2NG).length == 1)
  }

  "Her Majesty the Queen" should "have eight grandchildren" in {
    assert(grandChildrenOfQueen(elizabeth2).length == 8)
    assert(grandChildrenOfQueen(elizabeth2G).length == 8)

    // did not list all grandchildren
    assert(grandChildrenOfQueen(elizabeth2NG).length == 2)
  }

  "Her Majesty the Queen" should "have three great-grandchildren" in {
    assert(greatGrandChildrenOfQueen(elizabeth2).length == 3)
    assert(greatGrandChildrenOfQueen(elizabeth2G).length == 3)

    // did not list all great-grandchildren
    assert(greatGrandChildrenOfQueen(elizabeth2NG).length == 1)
  }
}
