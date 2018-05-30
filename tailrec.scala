import scala.annotation.tailrec

object Main {
	
	@tailrec def func1(n:Int, r: Int) : Int = {
		if (n < 10)
			r + func2(n, n-1)
		else
			func1(n/10, r + func2(n%10, (n%10)-1))
	}
	
	@tailrec def func2(n: Int, r: Int) : Int = {
		if (n <= 2)
			n
		else if (r == 1)
			n
		else
			func2(n * r, r - 1)
	}
	
    def test(n:Int) : Boolean = {
        if (n > 2)
        	func1(n, 0) == n
        else
        	false
    }


    def main(args:Array[String]) {
        println(test(2))
        println(test(145))
				println(test(99))
    }
}