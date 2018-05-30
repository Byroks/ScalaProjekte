object CharacterTree {
  
   def printTree(numberLetters: Int)  {
   	  
       if(numberLetters>1){
       printTree(numberLetters-1)
       print("\n")
       space(numberLetters)
     }
     else space(1)
     upTree(numberLetters,65)
     
     if (numberLetters>=2){
       
       downTree(numberLetters-1,63+numberLetters)
     }
     
   }
    
   def upTree(n: Int, pointer: Int)  {
   	
     print((pointer).toChar) 
     
      if (n>1) { upTree(n-1, pointer+1);
            
    }
      
   }
   
     
   def downTree(n: Int, pointer: Int)  {
   
      print((pointer).toChar)  
      
     if (n>1) downTree(n-1, pointer-1);
          
    }
      
    def space(n: Int)  {
      print(" ")
      if(n<26) space(n+1)
               
      }
        
   def main(args: Array[String]) {
     printTree(26);
   }
}