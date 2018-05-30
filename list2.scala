object list2 {

  def listToString (inputList: List[Int]) : String =
    inputList.foldRight("!") ((x,y) => x.toChar.toString.concat(y))

  def _kreuzProdukt(t1: (Int,Int,Int), t2: (Int,Int,Int)): (Int, Int, Int) = {
    (t1._2 * t2._3 - t1._3 * t2._2, t1._3 * t2._1 - t1._1 * t2._3, t1._1 * t2._2 - t1._2 * t2._1)
  }
  def kreuzProdukt(inputList: List[(Int, Int, Int)]): (Int, Int, Int) = {
    _kreuzProdukt(_kreuzProdukt(inputList(0),inputList(1)),inputList(2))
  }

  def main(args: Array[String]) {
    val testList: List[Int] = List(80, 114, 111, 103, 114, 97, 109, 109, 105, 101, 114, 112, 97, 114, 97, 100, 105, 103, 109, 101, 110)
    val vectorList: List[(Int,Int,Int)] = List((1,2,3),(4,5,6),(7,8,9))
    println(listToString(testList))
    println(kreuzProdukt(vectorList))
  }
}