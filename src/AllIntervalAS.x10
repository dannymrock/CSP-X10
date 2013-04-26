/** AllIntervalAS is the implementation of All-Intervals problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation
 * 	by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 	12 April, 2013 -> First Version
 */
public class AllIntervalAS extends ModelAS { 
	
	/** nb occurrences (to compute total cost) 0 is unused */
	val nbOcc : Array[Int];	
	val exh : Boolean;
	
	/**
	 * 	Constructor
	 *  @param lengthProblem Number of variables of the problem
	 * 	@param seed Desired seed for randomness of  the problem
	 */
	def this ( val lengthProblem : Int , seed : Long, exahustive:Boolean ){
		super( lengthProblem, seed );
		nbOcc = new Array[Int]( varRegion , 0 );
		exh = exahustive;
		initParameters();
		
	}
	
	
	private def initParameters(){
		if (exh){
			solverParams.probSelectLocMin = 66;
			solverParams.freezeLocMin = 1;
			solverParams.freezeSwap = 0;
			solverParams.resetLimit = 1;
			solverParams.resetPercent = 25;
			solverParams.restartLimit = 10000000;
			solverParams.restartMax = 0;
			solverParams.baseValue = 0;
			solverParams.exhaustive = true;
			solverParams.firstBest = true;
		}else{
			solverParams.probSelectLocMin = 6;
			solverParams.freezeLocMin = 5;
			solverParams.freezeSwap = 0;
			solverParams.resetLimit = length/6;
			solverParams.resetPercent = 10;
			solverParams.restartLimit = 10000000;
			solverParams.restartMax = 0;
			solverParams.baseValue = 0;
			solverParams.exhaustive = false;
			solverParams.firstBest = false;
		}
	} 
	
	/**
	 * 	Computes the cost of the solution
	 * 	@param nbOcc vector of occurrences
	 * 	@return cost
	 */
	public def cost() : Int 
	{
		var r : Int = 0;
		var i : Int = length;

		this.nbOcc(0) = 0;                /* 0 is unused, use it as a sentinel */

		while(this.nbOcc(--i) != 0 );//{
			//Console.OUT.print("nbOcc("+i+")= "+nbOcc(i));
		//}
			

		return i;
	}
	
	/**
	 * 	Returns the total cost of the current solution.
	 * 	Also computes errors on constraints.
	 * 	@param shouldBeRecorded 0 for no record 1 for record
	 * 	@return cost of solution
	 */
	public def costOfSolution( shouldBeRecorded : Int ) : Int
	{
		var i : Int;

		this.nbOcc.clear();
		
		for(i = 0; i < length - 1; i++){
			val aux = Math.abs(variables(i) - variables(i + 1)); 
			this.nbOcc(aux) = this.nbOcc(aux) + 1;
		}
		
		if (Is_Trivial_Solution(variables, length))
			return length;
		
		return cost();
	}
	
	public def Is_Trivial_Solution(sol : Array[Int], size : Int) : Boolean
	{
		return ( 	sol(0) == 0 || sol(0) == size - 1 || sol(size - 1) == 0 || sol(size - 1) == size - 1);
	}
	
	/**
	 *  costIfSwap(current_cost : Int, i1 : Int, i2 : Int) : Int
	 *  This function computes the cost of the problem if there is a swap between variable
	 *  i1 and i2.
	 * 	@param current_cost The current cost of the problem
	 *  @param i1 first variable to swap
	 *  @param i2 second variable to swap
	 *  @return cost of the problem if the swap is done
	 */
	public def costIfSwap(current_cost:Int,var i1:Int, var i2:Int):Int
	{
		var s1 : Int;
		var s2 : Int;
		var rem1 : Int;
		var rem2 : Int;
		var rem3 : Int;
		var rem4 : Int;
		var add1 : Int;
		var add2 : Int;
		var add3 : Int;
		var add4 : Int;

		if ((i1 == 0 && (variables(i2) == 0 || variables(i2) == length - 1)) ||
				(i2 == 0 && (variables(i1) == 0 || variables(i1) == length - 1)))
			return length;
		
		if(i2 < i1){
			val aux = i1;
			i1 = i2;
			i2 = aux;
		}else if(i1==i2){
			return cost();
		}
			

		s1 = variables(i1);
		s2 = variables(i2);

		if (i1 > 0)
		{
			rem1 = Math.abs(variables(i1 - 1) - s1); 
			this.nbOcc(rem1) = this.nbOcc(rem1) - 1; 
			add1 = Math.abs(variables(i1 - 1) - s2); 
			this.nbOcc(add1) = this.nbOcc(add1) + 1; 
		}
		else
			rem1 = add1 = 0;


		if (i1 < i2 - 1)		// i1 and i2 are not consecutive    ...if(Math.abs(i1-i2) > 1) 
		{	
			//Console.OUT.println("nocon");
			rem2 = Math.abs(s1 - variables(i1 + 1));
			this.nbOcc(rem2) = this.nbOcc(rem2) - 1; 
			add2 = Math.abs(s2 - variables(i1 + 1));
			this.nbOcc(add2) = this.nbOcc(add2) + 1; 

			rem3 = Math.abs(variables(i2 - 1) - s2); 
			this.nbOcc(rem3) = this.nbOcc(rem3) - 1; 
			add3 = Math.abs(variables(i2 - 1) - s1);
			this.nbOcc(add3) = this.nbOcc(add3) + 1; 
		}
		else
			rem2 = add2 = rem3 = add3 = 0;

		if (i2 < length - 1)
		{
			rem4 = Math.abs(s2 - variables(i2 + 1)); 
			this.nbOcc(rem4) = this.nbOcc(rem4) - 1;
			add4 = Math.abs(s1 - variables(i2 + 1));
			this.nbOcc(add4) = this.nbOcc(add4) + 1;
		}
		else
			rem4 = add4 = 0;

		var r : Int = cost();
		//Console.OUT.println("r = "+r);
		// undo 

		this.nbOcc(rem1) = this.nbOcc(rem1) + 1;
		this.nbOcc(rem2) = this.nbOcc(rem2) + 1;
		this.nbOcc(rem3) = this.nbOcc(rem3) + 1;
		this.nbOcc(rem4) = this.nbOcc(rem4) + 1; 
		this.nbOcc(add1) = this.nbOcc(add1) - 1;
		this.nbOcc(add2) = this.nbOcc(add2) - 1;
		this.nbOcc(add3) = this.nbOcc(add3) - 1;
		this.nbOcc(add4) = this.nbOcc(add4) - 1;

		return r;
	}
	
	/**
	 *  executedSwap( i1 : Int, i2 : Int)
	 *  This function updates the values of the object data structures for the problem due to the 
	 *  completion of a swap between two variables
	 *  @param i1 First variable already swapped
	 *  @param i2 Second variable already swapped
	 */
	public def executedSwap(var i1 : Int, var i2 : Int) {
		var s1 : Int;
		var s2 : Int;
		var rem1 : Int;
		var rem2 : Int;
		var rem3 : Int;
		var rem4 : Int;
		var add1 : Int;
		var add2 : Int;
		var add3 : Int;
		var add4 : Int;
		
		//if(!exh){
			//costOfSolution(1);
			//return;
		//}
		
		if(i2 < i1){
			val aux = i1;
			i1 = i2;
			i2 = aux;
		}
		//else if(i1==i2){
			//return cost();
		//}
		
		s1 = variables(i2);			// swap already executed 
		s2 = variables(i1);

		if (i1 > 0)
		{
			rem1 = Math.abs(variables(i1 - 1) - s1); 
			this.nbOcc(rem1) = this.nbOcc(rem1) - 1; 
			add1 = Math.abs(variables(i1 - 1) - s2); 
			this.nbOcc(add1) = this.nbOcc(add1) + 1; 
		}

		if (i1 < i2 - 1)              // i1 and i2 are not consecutive 
		{
			rem2 = Math.abs(s1 - variables(i1 + 1)); 
			this.nbOcc(rem2) = this.nbOcc(rem2) - 1; 
			add2 = Math.abs(s2 - variables(i1 + 1)); 
			this.nbOcc(add2) = this.nbOcc(add2) + 1; 

			rem3 = Math.abs(variables(i2 - 1) - s2);
			this.nbOcc(rem3) = this.nbOcc(rem3) - 1; 
			add3 = Math.abs(variables(i2 - 1) - s1); 
			this.nbOcc(add3) = this.nbOcc(add3) + 1; 
		}

		if (i2 < length - 1)
		{
			rem4 = Math.abs(s2 - variables(i2 + 1)); 
			this.nbOcc(rem4) = this.nbOcc(rem4) - 1;
			add4 = Math.abs(s1 - variables(i2 + 1)); 
			this.nbOcc(add4) = this.nbOcc(add4) + 1;
		}
		
		//Console.OUT.print("New nbOcc = ");
		//for (n in nbOcc)
			//Console.OUT.print(n+"->"+nbOcc(n));
		//Console.OUT.println(" ");
		
	}
	
	/**
	 * 	Reset function
	 * 	@param n number of variables to reset
	 * 	@param totalcost not used (for support more complex implementations)
	 * 	@return -1 for recompute cost
	 */
	public def reset( n : Int, totalCost : Int ): Int // AdData *p_ad
	{
		var distMin : Int = length - 3;	// size - 1 also works pretty well 
		var i : Int;
		var j : Int;
		
		for(i = 1; i < length; i++)
		{
			if (Math.abs(variables(i - 1) - variables(i)) >= distMin)
			{
				j = r.randomInt(length);
				this.swapVariables(i,j);
			}
		}
		return -1;
	}
	
	
	public def costOnVariable(var i:Int):Int{
		var costV : Int = 0;
		val miss = cost();
		
		if(variables(i) >= miss)
			costV += length;
		if(variables(i) < length-miss )
			costV += length;
		return costV;
		
		
// 		//var s1 : Int;
// 		var distL:Int = length;
// 		var distR:Int = length;
// 		
// 		if ((i == 0 && (variables(i) == 0 || variables(i) == length - 1)) ||
// 				(i == length - 1 && (variables(i) == 0 || variables(i) == length - 1)))
// 			return length;
// 
// 		//s1 = variables(i);
// 		
// 		if (i > 0){
// 			distL = Math.abs(variables(i) - variables(i-1));
// 		}
// 		if (i < length - 1 ){
// 			distR = Math.abs(variables(i) - variables(i+1));
// 		}
// 		
// 		if (distR < distL){
// 			return(length - distR);
// 		}else
// 			return(length - distL);
	}
}