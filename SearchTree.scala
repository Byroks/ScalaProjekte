//define functions every tree has
abstract class AbstractBinTree {
  // check if tree is empty
  def isEmpty: Boolean
  // get data
  def getData: Int
  //get left child
  def getLeft: AbstractBinTree
  //get right child
  def getRight: AbstractBinTree
  // getString
  def getString(indend: Int): String
  // use getString method for output
  override def toString = getString(1)
}

//represent empty Tree
case object EmptyTree extends AbstractBinTree {
  //an empty tree doesn't contain anything
  def isEmpty: Boolean = true
  def getData: Int = throw new RuntimeException("Cannot get data from empty tree")
  def getLeft: AbstractBinTree = throw new RuntimeException("Cannot get left tree from empty tree")
  def getRight: AbstractBinTree = throw new RuntimeException("Cannot get right tree from empty tree")
  def getString(indend: Int): String = ""
}

//an actual binary tree
//to enforce functional programming, all members are final
case class BinTree(data: Int, left: AbstractBinTree, right: AbstractBinTree) extends AbstractBinTree {
  def isEmpty: Boolean = false
  def getData: Int = data
  def getLeft: AbstractBinTree = left
  def getRight: AbstractBinTree = right
  def getString(indend: Int): String = {
    var dataPart = "|-" + data
    if (!(left isEmpty)) {
      dataPart += "\n" + "   " * indend + left.getString(indend + 1)
    }
    if (!(right isEmpty)) {
      dataPart += "\n" + "   " * indend + right.getString(indend + 1)
    }
    dataPart
  }
}

object SearchTree {

  //TODO: implement functionality to determine height of tree
  def height(tree: AbstractBinTree): Int = tree match{
    case EmptyTree => 0
    case BinTree(_, l, r) => 1 + Math.max(height(l), height(r))
  }

  def balance(tree: AbstractBinTree): Int = tree match{
    case EmptyTree => 0
    case BinTree(_, l, r) => height(r) - height(l)
  }

  //TODO: implement insertion of value into tree while keeping the tree balanced 
  def insert(value: Int, tree: AbstractBinTree): AbstractBinTree = balance(tree) match{
    case 0 if(tree.isEmpty) => BinTree(value, EmptyTree, EmptyTree)
    case 0                  => BinTree(tree.getData, insert(value, tree.getLeft), tree.getRight)
    case -1                 => BinTree(tree.getData, tree.getLeft, insert(value, tree.getRight))
    case 1                  => BinTree(tree.getData, insert(value, tree.getLeft), tree.getRight)
  }

  def __insert(tree: List[Int], t: AbstractBinTree): AbstractBinTree = tree match{
    case x :: Nil => insert(tree.head, t)
    case x :: xs  => insert(tree.head, __insert(tree.tail, t))
  }

  //TODO: write function, which deletes a value from tree and rebalance if neccessary
  def __delete(value: Int, tree: AbstractBinTree): List[Int] = tree match{
    case tree if(tree.getData==value && tree.getLeft!=EmptyTree && tree.getRight!=EmptyTree) => __delete(value, tree.getLeft) ::: __delete(value, tree.getRight)
    case tree if(tree.getData==value && tree.getLeft!=EmptyTree) => __delete(value, tree.getLeft)
    case tree if(tree.getData==value && tree.getRight!=EmptyTree) => __delete(value, tree.getRight)
    case tree if(tree.getData==value && tree.getLeft==EmptyTree && tree.getRight==EmptyTree) => Nil
    case tree if(tree.getLeft!=EmptyTree && tree.getRight!=EmptyTree) => tree.getData :: __delete(value, tree.getLeft) ::: __delete(value, tree.getRight)
    case tree if(tree.getLeft!=EmptyTree) => tree.getData :: __delete(value, tree.getLeft)
    case tree if(tree.getRight!=EmptyTree) => tree.getData :: __delete(value, tree.getRight)
    case tree if(tree.getLeft==EmptyTree && tree.getRight==EmptyTree) => tree.getData :: Nil
  }

  def delete(value: Int, tree: AbstractBinTree): AbstractBinTree = __insert(__delete(value, tree), EmptyTree)

  def main(args: Array[String]) {
    //Test your code!
    
    var tree1 = BinTree(10, BinTree(20, BinTree(30, EmptyTree, BinTree(40, EmptyTree, EmptyTree)), BinTree(50, EmptyTree, EmptyTree)), BinTree(60, EmptyTree, EmptyTree));

    println(tree1)
    println(delete(30, tree1))

    println(height(tree1))
  }
}