object Main {

    //implement stream generating primes
    val primeStream: Stream[Int] = 2 #:: Stream.from(3,2).filter(isPrime)

    def isPrime(n: Int): Boolean = (2 to math.sqrt(n).toInt).forall(n%_!=0)

    //write stream for values of the euler phi function
    val eulerPhi: Stream[Int] = 1 #:: Stream.from(2).map(i => eul(i))

    def eul(i: Int): Int = {
        if (isPrime(i)) {
            i - 1
        }
        else {
            1 to i count(gcd(i, _) == 1)
        }
    }

    def gcd(a: Int, b:Int): Int = (a, b) match {
        case (a, 0) => a
        case (a, b) => gcd(b, a % b)
    }

    //implement the dying rabits function
    val dyingRabits: Stream[Int] = 0 #:: Stream.from(1).map(i => rab(i))

    def rab(i: Int) : Int = i match {
        case 0 => 0
        case 1 => 1
        case _ => if (i <= 12) { rab(i-1) + rab(i-2)} else { rab(i-1) + rab(i-2) - rab(i-13)}
    }

    def main(args:Array[String]) =
    {
        println("Primes: ")
        primeStream take 10 foreach println
        println("eulersche phi-function: ")
        eulerPhi take 10 foreach println
        println("dying rabits: ")
        dyingRabits take 20 foreach println
     }
}