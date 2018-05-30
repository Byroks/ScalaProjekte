object FunctionHigherOrder {
  
  
  def strecken(func: Double => Double, k: Double): Double => Double = k * func(_)

  def strecken2(func: Double => Double, k: Double): Double => Double = { x: Double => k * func(x) }

  def moveGraph(func: Double => Double, x: Double, y: Double): Double => Double = {
    
       k: Double  => func(k-y)+x
         
   }
    
   
  def addFunc(func1: Double => Double, func2: Double => Double): Double => Double = {
    
   d: Double => func1(d) + func2(d)
    
  }
   

  def operateFunction(func: Double => Double, operator: (Double => Double) => (Double => Double)): Double => Double = 
  {
     operator(func(_))
    
  }
    
  def f1(x: Double): Double = x * x
  def f2(x: Double): Double = 4 * x + 7

  def main(args: Array[String]): Unit = {
    def g = strecken(f1, 2)
    def h = strecken2(f1, 2)
  
    def j = addFunc(f1,f2)
 //   println(j(1))
    
    def k = operateFunction(f1, strecken(_,2))
    println(k(1))
    
//    def i = moveGraph(f1,1,1)
//    println(i(2)) 
   
//    println(g(2))
//    println(g(4))
//    println(h(2))
//    println(h(2))
    
  }
}
