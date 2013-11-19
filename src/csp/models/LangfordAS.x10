package csp.models;

/** LangfordAS is the implementation of Langford pairing problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation
 * 	by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 22, 2013 First Version
 */

public class LangfordAS extends ModelAS{ 
	
	val order : Int;
	
	public def this (val lengthProblem : Int, seed : Long){
		super(lengthProblem * 2n, seed);
		order = lengthProblem;
		initParameters();
	}

	/**
	 * 	initParameters() 
	 *  Set Initial values for the problem
	 */
	private def initParameters(){
		solverParams.probSelectLocMin = 3n;
		solverParams.freezeLocMin = 1n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit = (order < 12n) ? 4n : 10n;
		//solverParams.resetPercent = 1;      //var to reset
		solverParams.nbVarToReset = 1n;
		solverParams.restartLimit = 100000n;
		solverParams.restartMax = 100n;
		solverParams.baseValue = 0n;
		solverParams.exhaustive = false;
		solverParams.firstBest = false; 
		
		solverParams.probChangeVector = 100n; //best 
		
		if (order % 4n != 0n && order % 4n != 3n)
		{
			Console.OUT.printf("no solution with size = %d\n", order);
			//exit(1);
		}
	} 
	
	public def costVar(i : Int) : Int {
		/* here i < order */
		var r : Int = 0n;
		var x : Int;
		var y : Int;
		var between : Int;

		x = variables(i);
		y = variables(order + i);

		between = Math.abs(x - y) - 1n;

		//#ifndef SLOW			/* the best !!! simply count 1 for an error */
		r = (between != i + 1n) ? 1n : 0n; //(between != i + 1);

		return r;
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
		var r : Int = 0n;

		for(i = 0n; i < order; i++)
			r += costVar(i);
		return r;
	}
	
	/**
	 *  Evaluates the error on a variable.
	 * 	@param i variable
	 * 	@return cost of variable i
	 */
	public def costOnVariable( var i : Int ) : Int
	{
		if (i >= order)
			i -= order;

		return costVar(i);
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
		var x : Int;
		var r : Int;

		x = variables(i1);
		variables(i1) = variables(i2);
		variables(i2) = x;

		r = costOfSolution(0n);

		variables(i2) = variables(i1);
		variables(i1) = x;

		//if (ad_reinit_after_if_swap)
			//Cost_Of_Solution(0);
		return r;
	}

	
	public def displaySolution()
	{
		var i : Int;
		var j : Int;
		
		//  Ad_Display(p_ad->sol, p_ad, NULL); // to see actual values
		for(i = 0n; i < length; i++)
		{
			for(j = 0n; variables(j) != i; j++)
				;
			if (j >= order)
				j -= order;
			Console.OUT.printf("%d ", j + 1);
		}
		Console.OUT.printf("\n");
	}
	
}