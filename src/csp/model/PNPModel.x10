package csp.model;
import csp.solver.Valuation;

public class PNPModel extends GenericModel{
	
	val size2 : Int;
	val sumMidX : Int;
	var curMidX : Int;
	val coeff : Int;
	val sumMidX2 : Long;
	var curMidX2 : Long;
	
	def this(sizeProblem:Long, seed:Long, opts:ParamManager): PNPModel(sizeProblem) {
		super(sizeProblem, seed, opts);
		size2 = (size / 2) as Int;
		
		if (size < 8 || size % 4 != 0)
		{
			Console.OUT.printf("no solution with size = %d\n", size);
			//exit(1);
		}
		
		sumMidX = ((size * (size + 1n)) / 4n) as Int;
		sumMidX2 = (sumMidX as Long * (2n * size + 1n)) / 3L;
		coeff = ( sumMidX2 / sumMidX ) as Int;
	}
	
	// /**
	//  * 	initParameters() 
	//  *  Set Initial values for the problem
	//  */
	// private def initParameters(rLimit:Int){
	// 	solverParams.probSelectLocMin = 80n;
	// 	solverParams.freezeLocMin = 1n;
	// 	solverParams.freezeSwap = 0n;
	// 	solverParams.resetLimit = 1n;
	// 	solverParams.resetPercent = 1n;
	// 	solverParams.restartLimit = rLimit;//(size < 100) ? 10 : (size < 1000) ? 150 : size / 10;
	// 	solverParams.restartMax = 100000n;
	// 	solverParams.baseValue = 1n;
	// 	solverParams.exhaustive = true;
	// 	solverParams.firstBest = false;
	// } 
	
	/**
	 * 	Returns the total cost of the current solution.
	 * 	Also computes errors on constraints for subsequent calls to
	 * 	Cost_On_Variable, Cost_If_Swap and Executed_Swap.
	 * 	@param shouldBeRecorded 0 for no record 1 for record
	 * 	@return cost of solution
	 */
	public def costOfSolution( shouldBeRecorded : Boolean ) : Long {
		var i : Int;
		var r : Long;
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
	public def costIfSwap( currentCost:Long, i1:Long, i2:Long ) : Long
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
	public def executedSwap( i1:Long, i2:Long )
	{
		var xi1 : Int, xi12 : Int, xi2 : Int, xi22 : Int;

		xi1 = variables(i2);		/* swap already executed */
		xi2 = variables(i1);

		xi12 = xi1 * xi1;
		xi22 = xi2 * xi2;

		curMidX = curMidX - xi1 + xi2;
		curMidX2 = curMidX2 - xi12 + xi22;
	}
	
	/**
	 *  CHECK_SOLUTION
	 * 
	 *  Checks if the solution is valid.
	 */

	public  def verify(conf:Valuation(sz)):Boolean {
		var sumA:Int = 0n, sumB:Int = 0n;
		var sumA2:Long = 0, sumB2:Long = 0;

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
		

		var i:Int;
		for(i = 0n; i < size2; i++)
		{
			sumA += conf(i);
			sumA2 += conf(i) * conf(i);
		}
		
		for(; i < size; i++)
		{
			sumB += conf(i);
			sumB2 += conf(i) * conf(i);
		}
		
		if (sumA != sumB)
		{
			Console.OUT.println("ERROR sum a: "+sumA+" != sum b: "+ sumB);
			return false;
		}

		if (sumA2 != sumB2)
		{
			Console.OUT.println("ERROR sum a^2: "+sumA2+" != sum b: "+sumB2);
			return false;
		}

		return true;
	}
	
	public def nextJ( i:Long, j:Long, exhaustive:Boolean ) : Long {
		return (j < 0) ? size2 as Long : j + 1;
	}
	
	
	public def nextI( i:Long ) : Long {
		val vari = i + 1;
		return vari < size2 as Long ? vari : size as Long;
	}

	
}
public type PNPModel(s:Long)=PNPModel{self.sz==s};
