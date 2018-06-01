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

object binarytree {

  def inOrder(tree: AbstractBinTree){
    if (tree != EmptyTree) {
      inOrder(tree.getLeft)
      print(tree.getData + " ")
      inOrder(tree.getRight)
    }
  }

  def map(tree: AbstractBinTree, func: Int => Int): AbstractBinTree = tree match{
    case tree if(tree.getLeft!=EmptyTree && tree.getRight!=EmptyTree) => BinTree(func(tree.getData), map(tree.getLeft, func), map(tree.getRight, func))
    case tree if(tree.getLeft!=EmptyTree) => BinTree(func(tree.getData), map(tree.getLeft, func), EmptyTree)
    case tree if(tree.getRight!=EmptyTree) => BinTree(func(tree.getData), EmptyTree, map(tree.getRight, func))
    case tree if(tree.getLeft==EmptyTree && tree.getRight==EmptyTree) => BinTree(func(tree.getData), EmptyTree, EmptyTree)
  }

  def fold(tree: AbstractBinTree, z: Int, f: (Int, Int) => Int): Int = tree match {
    case EmptyTree => z
    case BinTree(x,left,right) => f(f(fold(left,z,f),fold(right,z,f)),x)
  }

  def sumTree(tree: AbstractBinTree): Int = fold(tree, 0, _+_)

  def main(args: Array[String]) {
    var tree = BinTree(10, BinTree(20, BinTree(30, EmptyTree, BinTree(40, EmptyTree, EmptyTree)), BinTree(50, EmptyTree, EmptyTree)), BinTree(60, EmptyTree, EmptyTree));

    //Output should be: 30 40 20 50 10 60 ()
    println(inOrder(tree));

    var mappedTree = map(tree, 2 * _)
    //now: 60 80 40 100 20 120 ()
    println(inOrder(mappedTree));

    println(fold(tree, 0, _+_))

    //result should be: 210
    println(sumTree(tree))

    // test your code here
  }
}