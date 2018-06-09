import java.util.zip.CRC32

object list3 {

  //returns list with tuples of name and hash of the name
  def hashAndZip(nameList: List[String]): List[(String, String)] = {
    var test = new CRC32()
    test.update(nameList.head.getBytes())
    nameList match {
      case x::Nil => (nameList.head, test.getValue.toHexString) :: Nil
      case x::xs => (nameList.head, test.getValue.toHexString) :: hashAndZip(nameList.tail)
    }
  }

  def main(args: Array[String]) {
    // beliebteste Vornamen des Jahres 1904
    val names: List[String] = List("Gertrud", "Wilhelm", "Anna", "Walter", "Erna", "Hans", "Martha", "Carl", "Elisabeth", "Hermann")
    val zippedList = hashAndZip(names)
    println(zippedList)
    //List((Gertrud,6c541ac8), (Wilhelm,42e223f0), (Anna,816e1189), (Walter,8092c0d4), (Erna,1b30dcca), (Hans,4836876), (Martha,a309fe16), (Carl,b8fdbfdf), (Elisabeth,158bac6d), (Hermann,e08e4d99))
  }
}