// Examples from:
//
// [YJJGKO]
// Yakushev, Jeuring, Jansson, Gerdes, Kiselyov, Oliveira.
// Comparing datatype generic libraries in Haskell.

import language.implicitConversions
import org.scalatest._

// sanity test for binary tree generator
class BinTreeSanityTest extends FlatSpec with DataTypes {
  "completeBinaryTree" should "produce complete binary trees" in {
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
}

// this example is for correctness.
// @converMe macro doesn't make it easier to write
// due to not supporting mutually recursive data types.
class SelectSalary extends FlatSpec with DataTypes {

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

  "selectSalary" should "collect salary nodes in some order" in {
    val sortedSalaries = selectSalary(genCom).sortBy(_.amount)
    assert(sortedSalaries == List[Float](1000, 2000, 8000, 100000).map(Salary.apply))
  }
}
