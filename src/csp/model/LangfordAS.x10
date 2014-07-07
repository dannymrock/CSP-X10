package csp.model;
import csp.solver.Valuation;
import csp.util.Logger;

/** LangfordAS is the implementation of Langford pairing problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation
 * 	by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.2 July 2013 Order 3 implementation (based in langford3.c in the adaptive search code)
 */

/*                                                                                                                                            
 *  MODELING (Langford3.c Adaptive Search C implementation)                                                                                                                                 
 *                                                                                                                                            
 *  order = n of the problem                                                                                                                  
 *  size  = order * K (we solve Langford(K, order) for K = 2 or 3)                                                                            
 *  sol[] = values: sol[i] = j (j in 0..size-1) encodes one position of a given value v:                                                      
 *                                                                                                                                            
 *          value v = i % order + 1                                                                                                           
 *                                                                                                                                            
 *          if i < order   the 1st occurrence of v appears in position j                                                                      
 *          if i < 2*order the 2nd occurrence of v appears in position j                                                                      
 *                    else the 3rd occurrence of v appears in position j                                                                      
 *                                                                                                                                            
 *          for instance, a solution for L(2,3) is:                                                                                           
 *                                                                                                                                            
 *              i  : 0 1 2 3 4 5                                                                                                              
 *              v  : 1 2 3 1 2 3                                                                                                              
 *          sol[i] : 2 0 5 4 3 1 which represents the following sequence                                                                      
 *          sequen.: 2 3 1 2 1 3                                                                                                              
 *                                                                                                                                            
 *  In the rest of the code we use x = v - 1 (x in 0..order-1).                                                                               
 *  The error on a value x is stored in err[x].                                                                                               
 *  err[x] = 1 iff the distance between both occurrences of x is invalid                                                                      
 *  (i.e. the distance between indices are != x + 2)     
 */

public class LangfordAS(order:Long) extends ModelAS{ 
	
	val paramK:Long; // Should be an input parameter
	val err:Rail[Int];

	def this (order : Long, vectorSize: Long/*{self==2*order}*/, seed : Long, rLimit:Int) : LangfordAS(vectorSize){
		super(vectorSize, seed);
		property(order);
		paramK = vectorSize /order;
		err = new Rail[Int](order,0n);
		initParameters(rLimit);
	}

	/**
	 * 	initParameters() 
	 *  Set Initial values for the problem
	 */
	private def initParameters(rLimit:Int){ 
		solverParams.probSelectLocMin = 15n;
		solverParams.freezeLocMin = 4n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit = 3n; //(order < 12n) ? 4n : 10n;
		solverParams.resetPercent = 1n;      //var to reset
		//solverParams.nbVarToReset = 1n;
		solverParams.restartLimit = rLimit;
		solverParams.restartMax = 100n;
		solverParams.baseValue = 0n;
		solverParams.exhaustive = false;
		solverParams.firstBest = false; 
		
		if ((paramK == 2 && order % 4 != 0 && order % 4 != 3) ||
				(paramK == 3 && (order < 9 || (order % 9 != 0 && order % 9 != 1 && order % 9 != 8)))){
			Console.OUT.printf("no solution with size = %d\n", order);
			//exit(1);
		}

		
	} 
	
	
	
	
	/**
	 * 	Returns the total cost of the current solution.
	 * 	Also computes errors on constraints for subsequent calls to
	 * 	Cost_On_Variable, Cost_If_Swap and Executed_Swap.
	 * 	@param shouldBeRecorded 0 for no record 1 for record
	 * 	@return cost of solution
	 */
	public def costOfSolution( shouldBeRecorded : Boolean ) : Int {
		var i : Int;
		var r : Int = 0n;

		for(i = 0n; i < order; i++){
			val c = computeError(i);
			err(i) = c as Int;
			r += c;
		}
		return r;
	}
	
	/**
	 *  Evaluates the error on a variable.
	 * 	@param i variable
	 * 	@return cost of variable i
	 */
	public def costOnVariable( var i : Int ) : Int
	{
		val x = i % order;
		return err(x);
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
		var r : Int = currentCost;
		
		val x = i1 % order;           /* value to exchange (in 0..order - 1) */
		val y = i2 % order;
		// 		int tmp;
		// 
		// 		#ifdef CHECK
		// 		if (current_cost != Cost_Of_Solution(0))
		// 			printf("SEEMS 111 AN ERROR %d <=> %d: %d should be %d\n", i1, i2, current_cost, Cost_Of_Solution(0));
		// 		#endif
		// 
		// 		#if 1
		if (x == y)                   /* exchange the same value ? */
			return r;
		// 		#endif
		// 
		var tmp:Int = variables(i1);
		variables(i1) = variables(i2);
		variables(i2) = tmp;
		
		r -= err(x);
		r -= err(y);
		
		r += computeError(x);
		r += computeError(y);
		
		// 		#ifdef CHECK
		// 		if (current_cost !=  Cost_Of_Solution(0))
		// 			printf("SEEMS 222 AN ERROR %d <=> %d: %d should be %d\n", i1, i2, current_cost, Cost_Of_Solution(0));
		// 		#endif
		
		variables(i2) = variables(i1);
		variables(i1) = tmp;
		
		// 		#ifdef CHECK
		// 		Cost_Of_Solution(0);
		// 		#endif
		// 		 return current_cost;	
		return r;
	}

	
	public def displaySolution()
	{
		var i : Int;
		var j : Int;
		Console.OUT.printf("\n");
		
		for(i = 0n; i < length; i++){
			for(j = 0n; variables(j) != i; j++)
				;
			j %= order;
			Console.OUT.printf("%d ", j + 1);
		}
		Console.OUT.printf("\n");
	}
	
	
	public def displaySolution2(conf:Valuation(sz)){
		var i : Int;
		var j : Int;
		Console.OUT.printf("\n");
		
		for(i = 0n; i < length; i++){
			for(j = 0n; conf(j) != i; j++)
				;
			j %= order;
			Console.OUT.printf("%d ", j + 1);
		}
		Console.OUT.printf("\n");
	}
	
	/**
	 *  CHECK_SOLUTION
	 * 
	 *  Checks if the solution is valid.
	 */
	public  def verify(conf:Valuation(sz)):Boolean {
		var order:Long = length / paramK;
		var r:Long = 0;
		
		//Check Permutation
		val permutV = new Rail[Int](sz, 0n);
		val baseV = solverParams.baseValue;
		for (mi in conf.range()){
			val value = conf(mi);
			permutV(value-baseV)++;
			if (permutV(value-baseV)>1){
				Console.OUT.println("Error: Not valid permutation, value "+ value +" is repeated");
			}
		}
		
		val vSort = new Rail[Int](paramK,0n);
		var j:Long;
		for(var x:Long = 0; x < order; x++){
			var i:Long = x;
			r = 0;
			
			//Console.OUT.println("value x "+(x+1));
			
			for(c in vSort.range()){
				val ind = conf(i);
				//Console.OUT.println(" "+ind);
				i += order;
				for(j = c - 1; j >= 0 && ind < vSort(j); j--)
					;
				j++;
				for(var k:Long = c; k > j; k--)
					vSort(k) = vSort(k - 1);
				vSort(j) = ind;
			}
			
			// Console.OUT.print("\nSORTED: ");
			// for(c in vSort.range()){
			// 	val ind = vSort(c);
			// 	Console.OUT.print(" "+ind);
			// }
			
			
			for(var c:Long = 1; c < paramK; c++){
				val ind1 = vSort(c - 1);           /* index of the 1st occurrence */
				val ind2 = vSort(c);               /* index of the 2nd occurrence */

				val between = ind2 - ind1;
				r += (between - 2 != x)? 1:0;
			}
			
			//Console.OUT.println("r= "+r);			
			if (r != 0){
				Console.OUT.println("ERROR: the "+(x+1n)+" values  are missplaced!");
				return false;
			}
		}
		
		return true;
	}
	
	
	
	
	public def	computeError(x:Long):Long{            /* here x < order */
		var r:Long = 0n, i:Long = x;
 		//val sort = new Rail[Int](paramK, 0n);
		
		// Logger.info(()=>{"FOR: "+x});
		// Logger.info(()=>{"VALUES: "});

		var j:Long, c:Long;
		
		//Sort variables
		var ind1:Int = variables(i);
		var ind2:Int = variables(i+order);
		var ind3:Int = variables(i+order*2);
		var tmp:Int;
		if (ind1 < ind2) {
			if (ind3 < ind1){
				tmp = ind1;
				ind1 = ind3;
				ind3 = tmp;
			}
		} else {
			if (ind2 < ind3){
				tmp = ind1;
				ind1 = ind2;
				ind2 = tmp;
			} 
			else{
				tmp = ind1;
				ind1 = ind3;
				ind3 = tmp;
			} 
		} 
		if(ind3<ind2) {
			tmp = ind2;
			ind2 = ind3;
			ind3 = tmp;
		}
		
		// for(c = 0; c < paramK; c++){
		// 		val ind = variables(i);
		// 		//Logger.info(()=>{" "+ind});
		// 		i += order;
		// 		for(j = c - 1; j >= 0 && ind < sort(j); j--)
		// 			;
		// 		j++;
		// 		for(var k:Long = c; k > j; k--)
		// 			sort(k) = sort(k - 1);
		// 		sort(j) = ind;
		// 	}
		
		
		// Logger.info("\nSORTED: ");
		// 	for(c = 0; c < paramK; c++){
		// 		val ind = sort(c);
		// 		Logger.info(()=>{" "+ind});
		// 	}
		// 	
		
		val between1 = ind2 - ind1;
		val between2 = ind3 - ind2;
		
		r = ((between1 - 2 != x)? 1:0)+((between2 - 2 != x)? 1:0);
		
		//  		for(c = 1; c < paramK; c++){
		//  			val ind1 = sort(c - 1);           /* index of the 1st occurrence */
		//  			val ind2 = sort(c);               /* index of the 2nd occurrence */
		// 
		//  			val between = ind2 - ind1;
		//  			r += (between - 2 != x)? 1:0;
		//  		}
		//  
		//  		val valr=r;
		//Logger.info(()=>{"\nCOST = "+valr+"\n\n"});
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
		val x = i1 % order;
		val y = i2 % order;

		err(x) = computeError(x) as Int;
		err(y) = computeError(y) as Int;
	}
	
}
public type LangfordAS(s:Long)=LangfordAS{self.sz==s};