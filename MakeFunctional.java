public class MakeFunctional {

			
static public int getDivisorNum (int num){
	
	return getDivisorNumhelp(num,1,0);
	
		}
	
static public int getDivisorNumhelp (int num, int div, int zahler){
	
		if (div<=num){
			
			if (num%div==0){
				return getDivisorNumhelp(num,div+1,zahler+1);
			}
			else return getDivisorNumhelp(num,div+1,zahler);
		}
			
	return zahler;
}
		
	public static void main(String[] Args)
	{
		
		System.out.println("The number of divisors for 23 is " + getDivisorNum(23));
		
		
		System.out.println("The number of divisors for 1 is " + getDivisorNum(1));
		
		
		System.out.println("The number of divisors for 12 is " + getDivisorNum(12));
		
	
		System.out.println("The number of divisors for 99 is " + getDivisorNum(99));
	}
}
	