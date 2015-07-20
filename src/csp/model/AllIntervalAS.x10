package csp.model;
import csp.solver.Valuation;

/** AllIntervalAS is the implementation of All-Intervals problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation
 * 	by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 	12 April, 2013 -> First Version
 */
public class AllIntervalAS extends ModelAS{
	 
	/** nb occurrences (to compute total cost) 0 is unused */
	val nbOcc : Rail[Int];	
	//val exh : Boolean;
	
	/**
	 * 	Constructor
	 *  @param sizeProblem Number of variables of the problem
	 * 	@param seed Desired seed for randomness of  the problem
	 */
	def this (sizeProblem : Long , seed : Long, opts:ParamManager ):AllIntervalAS(sizeProblem){
		super( sizeProblem, seed, opts );
		this.nbOcc = new Rail[Int] (size , 0n);
	}
	
	
	
	// 	if (exh){
	// 		solverParams.probSelectLocMin = 66n;
	// 		solverParams.freezeLocMin = 1n;
	// 		solverParams.freezeSwap = 0n;
	// 		solverParams.resetLimit = 1n;
	// 		solverParams.resetPercent = 25n;
	// 		solverParams.restartLimit = rLimit;
	// 		solverParams.restartMax = 0n;
	// 		solverParams.baseValue = 0n;
	// 		solverParams.exhaustive = true;
	// 		solverParams.firstBest = true;
	// 	}else{
	// 		solverParams.probSelectLocMin = 6n;
	// 		solverParams.freezeLocMin = 5n;
	// 		solverParams.freezeSwap = 0n;
	// 		solverParams.resetLimit = size / 6n;
	// 		solverParams.resetPercent = 10n;
	// 		solverParams.restartLimit = rLimit;
	// 		solverParams.restartMax = 0n;
	// 		solverParams.baseValue = 0n;
	// 		solverParams.exhaustive = false;
	// 		solverParams.firstBest = false;
	// 	}
	
	/**
	 * 	Computes the cost of the solution
	 * 	@param nbOcc vector of occurrences
	 * 	@return cost
	 */
	public def cost() : Int 
	{
		var r : Int = 0n;
		var i : Int = size;

		this.nbOcc(0) = 0n;                /* 0 is unused, use it as a sentinel */

		while(this.nbOcc(--i) != 0n );//{
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
	public def costOfSolution( shouldBeRecorded : Boolean ) : Int
	{
		var i : Int;

		this.nbOcc.clear();
		
		for(i = 0n; i < size - 1; i++){
			val aux = Math.abs(variables(i) - variables(i + 1n)); 
			this.nbOcc(aux) = this.nbOcc(aux) + 1n;
		}
		
		if (isTrivialSolution(variables, size as Int))
			return size as Int;
		
		return cost();
	}
	
	public def isTrivialSolution(sol : Rail[Int], size : Int) : Boolean
	{
		return (sol(0) == 0n || sol(0) == size - 1n || sol(size - 1n) == 0n || sol(size - 1n) == size - 1n);
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
	public def costIfSwap( current_cost:Int,var i1:Long, var i2:Long ):Int
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

		if ((i1 == 0 && (variables(i2) == 0n || variables(i2) == size - 1n)) ||
				(i2 == 0 && (variables(i1) == 0n || variables(i1) == size - 1n)))
			return size;
		
		// if(i2 < i1){
		// 	val aux = i1;
		// 	i1 = i2;
		// 	i2 = aux;
		// }else if(i1==i2){
		// 	return cost();
		// }
			

		s1 = variables(i1);
		s2 = variables(i2);

		if (i1 > 0)
		{
			rem1 = Math.abs(variables(i1 - 1) - s1); 
			this.nbOcc(rem1) = this.nbOcc(rem1) - 1n; 
			add1 = Math.abs(variables(i1 - 1) - s2); 
			this.nbOcc(add1) = this.nbOcc(add1) + 1n; 
		}
		else
			rem1 = add1 = 0n;


		if (i1 < i2 - 1)		// i1 and i2 are not consecutive    ...if(Math.abs(i1-i2) > 1) 
		{	
			//Console.OUT.println("nocon");
			rem2 = Math.abs(s1 - variables(i1 + 1));
			this.nbOcc(rem2) = this.nbOcc(rem2) - 1n; 
			add2 = Math.abs(s2 - variables(i1 + 1));
			this.nbOcc(add2) = this.nbOcc(add2) + 1n; 

			rem3 = Math.abs(variables(i2 - 1) - s2); 
			this.nbOcc(rem3) = this.nbOcc(rem3) - 1n; 
			add3 = Math.abs(variables(i2 - 1) - s1);
			this.nbOcc(add3) = this.nbOcc(add3) + 1n; 
		}
		else
			rem2 = add2 = rem3 = add3 = 0n;

		if (i2 < size - 1)
		{
			rem4 = Math.abs(s2 - variables(i2 + 1)); 
			this.nbOcc(rem4) = this.nbOcc(rem4) - 1n;
			add4 = Math.abs(s1 - variables(i2 + 1));
			this.nbOcc(add4) = this.nbOcc(add4) + 1n;
		}
		else
			rem4 = add4 = 0n;

		var r : Int = cost();
		//Console.OUT.println("r = "+r);
		// undo 

		this.nbOcc(rem1) = this.nbOcc(rem1) + 1n;
		this.nbOcc(rem2) = this.nbOcc(rem2) + 1n;
		this.nbOcc(rem3) = this.nbOcc(rem3) + 1n;
		this.nbOcc(rem4) = this.nbOcc(rem4) + 1n; 
		this.nbOcc(add1) = this.nbOcc(add1) - 1n;
		this.nbOcc(add2) = this.nbOcc(add2) - 1n;
		this.nbOcc(add3) = this.nbOcc(add3) - 1n;
		this.nbOcc(add4) = this.nbOcc(add4) - 1n;

		return r;
	}
	
	/**
	 *  executedSwap( i1 : Int, i2 : Int)
	 *  This function updates the values of the object data structures for the problem due to the 
	 *  completion of a swap between two variables
	 *  @param i1 First variable already swapped
	 *  @param i2 Second variable already swapped
	 */
	public def executedSwap(var i1:Long, var i2:Long ) {
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
		
		//we know i1 < i2 due to ad.exhaustive */
		// else uncomment this
		// if(i2 < i1){
		// 	val aux = i1;
		// 	i1 = i2;
		// 	i2 = aux;
		// }
		
		
		//else if(i1==i2){
			//return cost();
		//}
		
		s1 = variables(i2);			// swap already executed 
		s2 = variables(i1);

		if (i1 > 0n)
		{
			rem1 = Math.abs(variables(i1 - 1) - s1); 
			this.nbOcc(rem1) = this.nbOcc(rem1) - 1n; 
			add1 = Math.abs(variables(i1 - 1) - s2); 
			this.nbOcc(add1) = this.nbOcc(add1) + 1n; 
		}

		if (i1 < i2 - 1n)              // i1 and i2 are not consecutive 
		{
			rem2 = Math.abs(s1 - variables(i1 + 1)); 
			this.nbOcc(rem2) = this.nbOcc(rem2) - 1n; 
			add2 = Math.abs(s2 - variables(i1 + 1)); 
			this.nbOcc(add2) = this.nbOcc(add2) + 1n; 

			rem3 = Math.abs(variables(i2 - 1) - s2);
			this.nbOcc(rem3) = this.nbOcc(rem3) - 1n; 
			add3 = Math.abs(variables(i2 - 1) - s1); 
			this.nbOcc(add3) = this.nbOcc(add3) + 1n; 
		}

		if (i2 < size - 1n)
		{
			rem4 = Math.abs(s2 - variables(i2 + 1)); 
			this.nbOcc(rem4) = this.nbOcc(rem4) - 1n;
			add4 = Math.abs(s1 - variables(i2 + 1)); 
			this.nbOcc(add4) = this.nbOcc(add4) + 1n;
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
	public def reset( n:Long, totalCost:Int ): Int // AdData *p_ad
	{
		var distMin : Int = size - 3n;	// size - 1 also works pretty well 
		var i : Int;
		var j : Int;
		
		for(i = 1n; i < size; i++)
		{
			if (Math.abs(variables(i - 1) - variables(i)) >= distMin)
			{
				j = r.randomInt(size);
				this.swapVariables(i,j);
			}
		}
		return -1n;
	}
	
	
	public def costOnVariable( i:Long ) : Int{
		var costV : Int = 0n;
		val miss = cost();
		
		if(variables(i) >= miss)
			costV += size;
		if(variables(i) < size-miss )
			costV += size;
		return costV;
		
		
// 		//var s1 : Int;
// 		var distL:Int = size;
// 		var distR:Int = size;
// 		
// 		if ((i == 0 && (variables(i) == 0 || variables(i) == size - 1)) ||
// 				(i == size - 1 && (variables(i) == 0 || variables(i) == size - 1)))
// 			return size;
// 
// 		//s1 = variables(i);
// 		
// 		if (i > 0){
// 			distL = Math.abs(variables(i) - variables(i-1));
// 		}
// 		if (i < size - 1 ){
// 			distR = Math.abs(variables(i) - variables(i+1));
// 		}
// 		
// 		if (distR < distL){
// 			return(size - distR);
// 		}else
// 			return(size - distL);
	}
	
		
	/**
	 *  CHECK_SOLUTION
	 * 
	 *  Checks if the solution is valid.
	 */
	public  def verify(conf:Valuation(sz)):Boolean {
 		var r:Int = 1n;
 		
 		//Check Permutation
 		val permutV = new Rail[Int](sz, 0n);
 		val baseV = this.baseValue;
 		for (mi in conf.range()){
 			val value = conf(mi);
 			permutV(value-baseV)++;
 			if (permutV(value-baseV)>1){
 				Console.OUT.println("Not valid permutation, value "+ value +" is repeted");
 			}
 		}
 
 		
 		nbOcc.clear();
 		var i:Int;
 		for(i = 0n; i < size - 1n; i++)
 			nbOcc(Math.abs(conf(i) - conf(i + 1)))++;
 
 		for(i = 1n; i < size; i++)
 			if (nbOcc(i) > 1)
 			{
 				Console.OUT.println("ERROR distance "+i+" appears "+nbOcc(i)+" times");
 				r = 0n;
 			} 
 		return r==1n;
 	}
	
}
public type AllIntervalAS(s:Long)=AllIntervalAS{self.sz==s};