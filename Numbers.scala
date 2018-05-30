
object Numbers {
  
  def printPrimes(from: Int, to: Int) {
             
      if (isPrime(from, 2)){
      print(from+" ")
      
    }
      if (from < to){
      printPrimes(from+1,to)
    
    }
  }
  
   def isPrime(check:Int, div:Int): Boolean= {
    
       if ( check%div == 0){
         return false
        }
       
       else if (div >= scala.math.sqrt(check)) {
       return true
     }
       else isPrime(check, div+1)
              
  }
  
  def printPGP(n: Int){ 
    
    if (n>20){
      printPGP(n-1);
    }
    else if(n>1){
       printPGP(n-1);
       print(n +": ")
     print(PGP(n) + " ")
     }

    
     
  }
    
   def PGP(n: Int): Int ={
     if (n==0){
      return 0;
      }
     if (n==1){
      return 1;
      }
       
      else {
        return PGP(n-1)+2*PGP(n-2)+5;
     } 
  }
    
  def main(args:Array[String]) {
//    printPrimes(1,17);
 printPGP(100);
    
  }  
}