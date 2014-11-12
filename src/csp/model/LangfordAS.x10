package csp.model;
import csp.solver.Valuation;
import csp.util.Logger;
import csp.util.Utils;

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
 *  (i.e. the distance between indices are != x + 2  and x + 1 for Skolem)     
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
		solverParams.probSelectLocMin = 6n;
		solverParams.freezeLocMin = 2n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit = 2n; //(order < 12n) ? 4n : 10n;
		solverParams.resetPercent = 1n;      //var to reset
		//solverParams.nbVarToReset = 1n;
		solverParams.restartLimit = rLimit;
		solverParams.restartMax = 1000n;
		solverParams.baseValue = 0n;
		solverParams.exhaustive = false;
		solverParams.firstBest = true; 
		
		if ((paramK == 2 && order % 4 != 0 && order % 4 != 3) ||
				(paramK == 3 && (order < 9 || (order % 9 != 0 && order % 9 != 1 && order % 9 != 8)))){
			Console.OUT.printf("no solution with size = %d\n", order);
			//exit(1);
		}

		
	} 
	
	public def	computeError(x:Long):Long{            /* here x < order */
		 var r:Long = 0n;
		 
		 // Sort variables
		 var ind1 : Int, ind2 : Int, ind3 : Int;
		 
		 // indexes
		 var i1 : Long = x;
		 var i2 : Long = i1 + order;
		 var i3 : Long = i2 + order;
		 //Utils.show("vector in compError",variables);	 
		 //Console.OUT.println("i1 "+i1+" i2 "+i2+" i3 "+i3);
		 
		 if (variables(i1) < variables(i2)) {
			  if (variables(i3) < variables(i1)){
					ind1 = variables(i3);
					ind2 = variables(i1);
					ind3 = variables(i2);
			  } else if (variables(i3) < variables(i2)) {
					ind1 = variables(i1);
					ind2 = variables(i3);
					ind3 = variables(i2);
			  } else{
					ind1 = variables(i1);
					ind2 = variables(i2);
					ind3 = variables(i3);
			  }
		 } else {
			  if (variables(i3) < variables(i2)){
					ind1 = variables(i3);
					ind2 = variables(i2);
					ind3 = variables(i1);
			  } else if (variables(i3) < variables(i1)) {
					ind1 = variables(i2);
					ind2 = variables(i3);
					ind3 = variables(i1);
			  } else {
					ind1 = variables(i2);
					ind2 = variables(i1);
					ind3 = variables(i3);			
			  }
		 } 
		 
		 if ((ind2 - ind1) != ( x as Int + 2n)) r++;   // x as Int + 1n for Skolem
		 if ((ind3 - ind2) != ( x as Int + 2n)) r++;
		 
		 //Console.OUT.println("ind1 "+ind1+" ind2 "+ind2+" ind3 "+ind3+ " r "+r);
		 return r;
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

		//Utils.show("vector ",variables);
		
		for(i = 0n; i < order; i++){
			val e = computeError(i);
			err(i) = e as Int;
			r += e;
		}

		//Console.OUT.println("error "+r);
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
		return (err(x) != 0n ? 1n : 0n); /* for K == 3 return if the variable is in error (i.e. 1 or 2 errors are the same) */
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

		if (x == y)        /* exchange the same value ? */
			return r + 1n;  /* for K == 3 don't return current_cost to avoid "false" plateau */

		var tmp:Int = variables(i1);
		variables(i1) = variables(i2);
		variables(i2) = tmp;
		
		r -= err(x);
		r -= err(y);
		
		r += computeError(x);
		r += computeError(y);
		
		variables(i2) = variables(i1);
		variables(i1) = tmp;
		
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
	
	
	public def displaySolution(conf:Valuation(sz)){
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
		
	/**
	 *  Compute distance between 2 configurations according Langford Model
	 */
	public def distance(conf1 : Valuation(sz), conf2 : Valuation(sz)) : Double {
		var eqNb:Double = 0;
		
		for(var v:Int = 0n; v < order; v++){
			
			val ind11:Int = conf1(v);
			val ind12:Int = conf1(v + order);
			val ind13:Int = conf1(v + order + order);
			
			val ind21:Int = conf2(v);
			val ind22:Int = conf2(v + order);
			val ind23:Int = conf2(v + order + order);
			var z:Int = 0n;		
			if(ind11 == ind21 || ind11 == ind22 || ind11 == ind23)
				z++;//eqNb++;
			if(ind12 == ind21 || ind12 == ind22 || ind12 == ind23)
				z++;//eqNb++;
			if(ind13 == ind21 || ind13 == ind22 || ind13 == ind23)
				z++;//eqNb++;
			if (z == 3n) eqNb++;
			//if (z == 2n) eqNb+=0.5;
		} 
		//Console.OUT.println("conf1");
		//displaySolution(conf1);
		//Console.OUT.println("conf2");
		//displaySolution(conf2);
		val dis = 1.0-(eqNb / order);//sz);
		// Console.OUT.println("number of coincidences "+ eqNb+" distance in LangfordAS = "+dis);
		// /*if (eqNb >= 4.0)*/ {
		// 	displaySolution(conf1);
		// 	displaySolution(conf2);
		// }
			
		return dis;
	}
	
}
public type LangfordAS(s:Long)=LangfordAS{self.sz==s};