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
    case BinTree(_, l, r) => height(l) - height(r)
  }

  //TODO: implement insertion of value into tree while keeping the tree balanced 
  def insert(value: Int, tree: AbstractBinTree): AbstractBinTree =
    if(tree!=EmptyTree){
      if(tree.getData>value)
        insert(value, tree.getLeft)
      else
        insert(value, tree.getRight)
    }
    else BinTree(value, EmptyTree, EmptyTree)
  
  //TODO: write function, which deletes a value from tree and rebalance if neccessary
  //def delete(value: Int, tree: AbstractBinTree): AbstractBinTree = ???

  def main(args: Array[String]) {
    //Test your code!
    
    var tree1 = BinTree(15, BinTree(20, BinTree(30, EmptyTree, BinTree(40, EmptyTree, EmptyTree)), BinTree(50, EmptyTree, EmptyTree)), BinTree(60, EmptyTree, EmptyTree));
    println(tree1)
    println(height(tree1))


  }
}