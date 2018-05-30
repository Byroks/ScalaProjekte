trait Person
case class ReferatsleiterIn(name: String, uedp: List[Person]) extends Person
case class ReferentIn(name: String, uedp: List[Person]) extends Person
case class SachbearbeiterIn(name: String, uedp: List[Person]) extends Person
case class MitarbeiterIn(name: String, uedp: List[Person]) extends Person

object tree {
  def determineType(x: Person): String = x match {
    case _:ReferatsleiterIn => "ReferatsleiterIn"
    case _:ReferentIn => "ReferentIn"
    case _:SachbearbeiterIn => "SachbearbeiterIn"
    case _:MitarbeiterIn => "MitarbeiterIn"
  }
  def determineSlave(x: Person): List[Person] = x match {
    case ReferatsleiterIn(name,uedp) => uedp
    case ReferentIn(name,uedp) => uedp
    case SachbearbeiterIn(name,uedp) => uedp
    case MitarbeiterIn(name,uedp) => uedp
  }

  def getEinfacheSachbearbeiter(list: List[Person], ret: List[Person]): List[Person] = list match {
    case Nil => ret
    case x::l if(determineType(x) == "SachbearbeiterIn"&& determineSlave(x) == List()) => getEinfacheSachbearbeiter(l, x::ret)
    case x::l => getEinfacheSachbearbeiter(l,ret)
  }

  val mit1 = new MitarbeiterIn("Chris", List())
  val mit2 = new MitarbeiterIn("Simon", List())
  val mit3 = new MitarbeiterIn("Alexa", List())
  val mit4 = new MitarbeiterIn("Sina", List())
  val sach1 = new SachbearbeiterIn("Karl",List(mit1))
  val sach2 = new SachbearbeiterIn("Kevin",List(mit2))
  val sach3 = new SachbearbeiterIn("Kim",List())
  val sach4 = new SachbearbeiterIn("Kerstin",List(mit3,mit4))
  val sach5 = new SachbearbeiterIn("Kiron",List())
  val ref1 = new ReferentIn("Paul",List(sach1,sach2,sach3))
  val ref2 = new ReferentIn("Johannes",List(sach4,sach5))
  val refL1 = new ReferatsleiterIn("Prof. Ortmeier",List(ref1,ref2))
  val pers: List[Person] = List(mit1, mit2, mit3, mit4, sach1, sach2, sach3, sach4, sach5, ref1, ref2, refL1)

  def main(args: Array[String]) {
    println(getEinfacheSachbearbeiter(pers,List())) //a
    println(determineSlave(pers(11))) //b Im Prinzip zeigt er hier auch alle anderen Untergebenen

  }
}