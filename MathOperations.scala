object MathOperations {

    def succ(x: Int): Int = x+1;
    
    def neg(x: Int): Int = -x;

    def add(x: Int, y:Int): Int = {

       if(x==0) return y
       if(y==0) return x
       else if(x>0 &&y>0) add(succ(x), neg(succ(neg(y))))
       else if(x>0 &&y<0) add(neg(succ(neg(x))), succ(y))
       else if(x<0 &&y>0) neg(add(neg(x), neg(y)))
       else neg(add(neg(x), neg(y)))
    }
    
    
    def sub(x: Int, y:Int): Int = {
       return (add((x), neg(y)))
    }
    
    def mul(x: Int, y:Int): Int = {
     if (y==1) return x
     if (y==0) return 0
     if (y<0) add(neg(x),mul(x,succ(y)))
     else add(x,mul(x,sub(y,1)))
     
}


 def div(x: Int, y:Int): Int = {
   if(x>0&&y>0) divhelp(x,y,0)
   else if(x>0&&y<0) neg(divhelp(x, neg(y), 0))
   else if(x<0&&y>0) neg(divhelp(neg(x), y, 0))
   else divhelp(neg(x), neg(y), 0)
 }
    
def divhelp(x: Int, y: Int, c: Int): Int ={
  if (x<y) c
  
  else divhelp(sub(x,y),y,succ(c))
  
}
     def main(args: Array[String]): Unit = {
        print(div(12, 2))
    }
}