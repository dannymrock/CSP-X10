package csp.models;

public class PartitAS extends ModelAS{
	
	val size2 : Int;
	
	val sumMidX : Int;
	var curMidX : Int;
	
	val coeff : Int;
	
	val sumMidX2 : Long;
	var curMidX2 : Long;
	
	public def this(val lengthProblem : Int, seed : Long){
		super(lengthProblem, seed);
		size2 = length / 2n;
		
		if (length < 8n || length % 4n != 0n)
		{
			Console.OUT.printf("no solution with size = %d\n", length);
			//exit(1);
		}
		
		sumMidX = (length * (length + 1n)) / 4n;
		sumMidX2 = (sumMidX as Long * (2n * length + 1n)) / 3L;
		coeff = ( sumMidX2 / sumMidX ) as Int;
		initParameters();
	}
	
	/**
	 * 	initParameters() 
	 *  Set Initial values for the problem
	 */
	private def initParameters(){
		solverParams.probSelectLocMin = 80n;
		solverParams.freezeLocMin = 1n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit = 1n;
		solverParams.resetPercent = 1n;
		solverParams.restartLimit = 100n;//(length < 100) ? 10 : (length < 1000) ? 150 : length / 10;
		solverParams.restartMax = 100000n;
		solverParams.baseValue = 1n;
		solverParams.exhaustive = true;
		solverParams.firstBest = false;
		
		solverParams.probChangeVector = 100n; //seems to be the best (no tested yet)
	} 
	
	/**
	 * 	Returns the total cost of the current solution.
	 * 	Also computes errors on constraints for subsequent calls to
	 * 	Cost_On_Variable, Cost_If_Swap and Executed_Swap.
	 * 	@param shouldBeRecorded 0 for no record 1 for record
	 * 	@return cost of solution
	 */
	public def costOfSolution( shouldBeRecorded : Int ) : Int {
		var i : Int;
		var r : Int;
		var x : Int;

		curMidX = 0n;
		curMidX2 = 0L;
		
		for(i = 0n; i < size2; i++)
		{
			x = variables(i);
			curMidX += x;
			curMidX2 += x * x;
		}

		r = coeff * Math.abs(sumMidX - curMidX) + (Math.abs(sumMidX2 - curMidX2) as Int);

		return r;
	}
	
	
	/**
	 * 	Evaluates the new total cost for a swap
	 * 	@param currentCost not used
	 * 	@param i1 first variable to swap
	 * 	@param i2 second variable to swap
	 * 	@return cost if swap
	 */
	public def costIfSwap(currentCost : Int, i1 : Int, i2 : Int) : Int
	{
		var xi1 : Int, xi12 : Int, xi2 : Int, xi22 : Int, cmX : Int, cmX2 : Long, r : Int;

		//#if 0				/* useless with customized Next_I and Next_J */
		if (i1 >= size2 || i2 < size2)
			return x10.lang.Int.MAX_VALUE;
		//#endif

		xi1 = variables(i1);
		xi2 = variables(i2);

		xi12 = xi1 * xi1;
		xi22 = xi2 * xi2;

		cmX = curMidX - xi1 + xi2;
		cmX2 = curMidX2  - (xi12 as Long ) + (xi22 as Long);
		r = coeff * Math.abs(sumMidX - cmX) + (Math.abs(sumMidX2 - cmX2) as Int);

		return r;
	}
	
	/**
	 * 	Records a swap
	 * 	@param i1 not used
	 * 	@param i2 not used
	 */
	public def executedSwap(i1:Int, i2:Int)
	{
		var xi1 : Int, xi12 : Int, xi2 : Int, xi22 : Int;

		xi1 = variables(i2);		/* swap already executed */
		xi2 = variables(i1);

		xi12 = xi1 * xi1;
		xi22 = xi2 * xi2;

		curMidX = curMidX - xi1 + xi2;
		curMidX2 = curMidX2 - xi12 + xi22;
	}
}