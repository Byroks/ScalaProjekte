object list {

  val bausteine_1 = Map(0 -> "", 1 -> "eins", 2 -> "zwei",3 -> "drei",4 -> "vier",5 -> "fünf", 6 -> "sechs",
    7 -> "sieben",8 -> "acht",9 -> "neun", 10 ->"zehn ",11 -> "elf",12 -> "zwölf",13 -> "dreizehn",
    14 -> "vierzehn",15 -> "fünfzehn",16 -> "sechszehn",17 -> "siebzehn",18 -> "achtzehn", 19 -> "neunzehn")

  val bausteine_2 = Map (0 -> "", 2 -> "zwanzig",3 -> "dreißig",4 -> "vierzieg",5 -> "fünfzig",6 -> "sechzig",
    7 -> "siebzig",8 -> "achtzig",9 -> "neunzig")

  val bausteine_3 = Map(0 -> "", 1 -> "einhundert", 2 -> "zweihundert", 3 -> "dreihundert", 4 -> "vierhundert", 5 -> "fünfhundert",
    6 -> "sechshundert", 7 -> "siebenhundert", 8 -> "achthundert", 9 -> "neunhudnert")

  val bausteine_4 = Map(0 -> "", 1 -> "eintausend", 2 -> "zweitausend", 3 -> "dreitausend", 4 -> "viertausend", 5 -> "fünftausend", 6 -> "sechstausend",
    7 -> "siebentausend", 8 -> "achttausend", 9 -> "neuntausend")

  //a)
  def grouping[T](xs: List[T], rule: T => Boolean): Tuple2[List[T], List[T]] =
    (xs.filter(rule(_)), xs.filter(!rule(_)))

  //b)
  def listebauen(n: Int, accum: List[Int]) : List[Int] = {
    n match {
      case 0 => accum
      case _ => listebauen(n / 10, (n % 10) :: accum)
    }
  }

  def listebauen(n: Int): List[Int] = listebauen(n, List())

  def translate(ln: List[Int]): List[String] = {
    val list_n = listebauen(ln.head)

    def __translate(ln: List[Int], accum: String): String = {
      ln.size match {
        case 4 => __translate(ln.tail, accum + bausteine_4(ln.head))
        case 3 => __translate(ln.tail, accum + bausteine_3(ln.head))
        case 2 if ln.head == 1 => accum + bausteine_1(10 + ln(1))
        case 2 if ln.head == 0 => accum + bausteine_1(ln(1))
        case 2 if ln.head >= 2 => accum + bausteine_1(ln(1)) + "und" + bausteine_2(ln.head)
        case 1 => accum + bausteine_1(ln.head)
      }
    }
    if(ln.tail != Nil) (__translate(list_n, "") :: translate(ln.tail))
    else __translate(list_n, "") :: Nil
  }

  def main(args: Array[String]) {
    println(grouping(List(1, 2, 100, 3, 4, 501, 12), (x: Int) => x >= 100))
    println(grouping(List('A', 'C', 'Z', 'T', 'O', 'P', 'N', 'M', 'Y'), (x: Char) => x >= 'J'))

    println(translate(List(1, 2, 3, 4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)))
    println(translate(List(20,30,40,50,60,70,80,90)))
    println(translate(List(100,200,300,400,500,600,700,800,900)))
    println(translate(List(1000,2000,3000,4000,5000,6000,7000,8000,9000)))
    println(translate(List(120,1999,9999,999,99,42,21,123,345)))
  }
}