/** AllIntervalAS is the implementation of All-Intervals problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  This code is an adaptation in x10 of the C implementation of Adaptive Search algoritm 
 * 	by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 	12 April, 2013 -> First Version
 */
public class AllIntervalAS extends ModelAS {
	
	/** nb occurrences (to compute total cost) 0 is unused */
	val nbOcc : Array[Int];		
	
	/**
	 * 	Constructor
	 *  @param lengthProblem Number of variables of the problem
	 * 	@param seed Desired seed for randomness of  the problem
	 */
	def this ( val lengthProblem : Int , seed : Long ){
		super( lengthProblem, seed );
		nbOcc = new Array[Int]( varRegion , 0 );
		initParameters();
	}
	
	
	private def initParameters(){
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
	} 
	
	/**
	 * 	Computes the cost of the solution
	 * 	@param nbOcc vector of occurrences
	 * 	@return cost
	 */
	public def cost(nbOcc : Array[Int]) : Int 
	{
		var r : Int = 0;
		var i : Int = length;

		nbOcc(0) = 0;                /* 0 is unused, use it as a sentinel */

		while(nbOcc(--i) != 0 )
			;

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

		nbOcc.clear();
		
		for(i = 0; i < length - 1; i++){
			val aux = Math.abs(variables(i) - variables(i + 1)); 
			nbOcc(aux) = nbOcc(aux) + 1;
		}
		
		if (Is_Trivial_Solution(variables, length))
			return length;
		
		return cost(nbOcc);
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
	public def costIfSwap(current_cost:Int, i1:Int, i2:Int):Int
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

		s1 = variables(i1);
		s2 = variables(i2);

		if (i1 > 0)
		{
			rem1 = Math.abs(variables(i1 - 1) - s1); 
			nbOcc(rem1) = nbOcc(rem1) - 1; 
			add1 = Math.abs(variables(i1 - 1) - s2); 
			nbOcc(add1) = nbOcc(add1) + 1; 
		}
		else
			rem1 = add1 = 0;


		if (i1 < i2 - 1)		// i1 and i2 are not consecutive 
		{
			rem2 = Math.abs(s1 - variables(i1 + 1));
			nbOcc(rem2) = nbOcc(rem2) - 1; 
			add2 = Math.abs(s2 - variables(i1 + 1));
			nbOcc(add2) = nbOcc(add2) + 1; 

			rem3 = Math.abs(variables(i2 - 1) - s2); 
			nbOcc(rem3) = nbOcc(rem3) - 1; 
			add3 = Math.abs(variables(i2 - 1) - s1);
			nbOcc(add3) = nbOcc(add3) + 1; 
		}
		else
			rem2 = add2 = rem3 = add3 = 0;

		if (i2 < length - 1)
		{
			rem4 = Math.abs(s2 - variables(i2 + 1)); 
			nbOcc(rem4) = nbOcc(rem4) - 1;
			add4 = Math.abs(s1 - variables(i2 + 1));
			nbOcc(add4) = nbOcc(add4) + 1;
		}
		else
			rem4 = add4 = 0;

		var r : Int = cost(nbOcc);

		// undo 

		nbOcc(rem1) = nbOcc(rem1) + 1;
		nbOcc(rem2) = nbOcc(rem2) + 1;
		nbOcc(rem3) = nbOcc(rem3) + 1;
		nbOcc(rem4) = nbOcc(rem4) + 1; 
		nbOcc(add1) = nbOcc(add1) - 1;
		nbOcc(add2) = nbOcc(add2) - 1;
		nbOcc(add3) = nbOcc(add3) - 1;
		nbOcc(add4) = nbOcc(add4) - 1;

		return r;
	}
	
	/**
	 *  executedSwap( i1 : Int, i2 : Int)
	 *  This function updates the values of the object data structures for the problem due to the 
	 *  completion of a swap between two variables
	 *  @param i1 First variable already swapped
	 *  @param i2 Second variable already swapped
	 */
	public def executedSwap( i1 : Int, i2 : Int) {
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

		s1 = variables(i2);			// swap already executed 
		s2 = variables(i1);

		if (i1 > 0)
		{
			rem1 = Math.abs(variables(i1 - 1) - s1); 
			nbOcc(rem1) = nbOcc(rem1) - 1; 
			add1 = Math.abs(variables(i1 - 1) - s2); 
			nbOcc(add1) = nbOcc(add1) + 1; 
		}

		if (i1 < i2 - 1)              // i1 and i2 are not consecutive 
		{
			rem2 = Math.abs(s1 - variables(i1 + 1)); 
			nbOcc(rem2) = nbOcc(rem2) - 1; 
			add2 = Math.abs(s2 - variables(i1 + 1)); 
			nbOcc(add2) = nbOcc(add2) + 1; 

			rem3 = Math.abs(variables(i2 - 1) - s2);
			nbOcc(rem3) = nbOcc(rem3) - 1; 
			add3 = Math.abs(variables(i2 - 1) - s1); 
			nbOcc(add3) = nbOcc(add3) + 1; 
		}

		if (i2 < length - 1)
		{
			rem4 = Math.abs(s2 - variables(i2 + 1)); 
			nbOcc(rem4) = nbOcc(rem4) - 1;
			add4 = Math.abs(s1 - variables(i2 + 1)); 
			nbOcc(add4) = nbOcc(add4) + 1;
		}
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
}