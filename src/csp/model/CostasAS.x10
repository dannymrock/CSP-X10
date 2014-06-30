package csp.model;
import csp.util.Logger;
import csp.solver.Valuation;

/** CostasAS is the implementation of Costas Array problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation
 * 	by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 10, 2013 First Version
 */

public class CostasAS extends ModelAS{  
	
	var size2:Long;
	var sizeSq:Long;
	/** nb occurrences of each diff (translated) */
	val nbOcc : Rail[Int];		/* diff are in -(size-1)..-1 1..size-1 */
	/* translated are in 0..2*size-1 [0] and [N] being unused */						
	/** records the indice of a first occurence of a (translated) difference */
	val first : Rail[Int];
	/** errors on variables */
	val err = new Rail[Int] (sz , 0n); 
	
	/* for reset: */
	/** save the sol[] vector */
	val saveSol = new Rail[Int] (sz, 0n);
	/** save the best sol[] found in a reset phase */
	val bestSol = new Rail[Int] (sz, 0n);
	/** indices of erroneous vars */
	var iErr:Rail[Int]{self.size==sz,self!=null} = new Rail[Int] (sz, 0n);			
	
	val toAdd : Rail[Int];
	
	//val length2reg : Region(1);
	
	val length2 : Long;
	/**
	 * 	Constructor
	 *  @param lengthProblem Number of variables of the problem
	 * 	@param seed Desired seed for randomness of the problem
	 */
	def this (lengthProblem : Long, seed : Long, rLimit:Int) : CostasAS(lengthProblem) {
		super(lengthProblem, seed);
		size2 = (lengthProblem - 1n) / 2n;		
		sizeSq = lengthProblem * lengthProblem;
		//length2reg = 0..((length * 2)-1);
		length2 = lengthProblem * 2n;
		
		nbOcc = new Rail[Int] (length2 , 0n);
		first =  new Rail[Int] (length2 , 0n);
		toAdd = new Rail[Int](10);
		initParameters(rLimit);
		Logger.info(()=>{"Starting CAP"});
	}

	/**
	 * 	initParameters() 
	 *  Set Initial values for the problem
	 */
	private def initParameters(rLimit:Int){
		solverParams.probSelectLocMin = 50n;
		solverParams.freezeLocMin = 1n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit = 1n;
		//solverParams.resetLimit = 2n;
		solverParams.resetPercent = 5n;
		solverParams.restartLimit = rLimit;
		solverParams.restartMax = 0n;
		solverParams.baseValue = 1n;
		solverParams.exhaustive = false;
		solverParams.firstBest = false;
		
		toAdd(0) = 1n; toAdd(1) = 2n; toAdd(2) = length - 2n; toAdd(3) = length - 3n;
	} 
	
	/**
	 * 	Computes the cost of the solution
	 * 	@param err vector of error on variables (if null don't update object err vector)
	 * 	@return cost
	 */
	def cost( err : Rail[Int] ) : Int {		//int *err)	 //Inline
		var dist : Int = 1n;
		var i : Int;
		var firstI : Int;
		var diff : Int;
		var diffTranslated : Int;
		var nb : Int;
		var r : Int = 0n;

		if (err != null) 
			err.clear();	//memset(err, 0, size * sizeof(int));

		do
		{
			nbOcc.clear();
			i = dist;
			val penalty  = (sizeSq - (dist * dist)) as Int;
			//var penalty : Int = 1;
			//var penalty : Int = sizeSq - dist;
			do
			{
				diff = variables( i - dist ) - variables(i);
				diffTranslated = diff + length;
				nbOcc(diffTranslated) = nbOcc(diffTranslated) + 1n;
				nb = nbOcc(diffTranslated);

				if (err != null) //if (err) ????
				{
					if (nb == 1n) 
						first(diffTranslated) = i;
					else
					{
						if (nb == 2n)
						{
							firstI = first(diffTranslated);
							//ErrOn(first_i);
							err(firstI) += penalty;
							err(firstI - dist) += penalty; 
						}
						//ErrOn(i);
						err(i) += penalty;
						err(i - dist) += penalty;
					}  
				}

				if (nb > 1)
					r += penalty;
			}while(++i < length);
		}while(++dist <= size2);
		return r;
	}
	/**
	 *  Evaluates the error on a variable.
	 * 	@param i variable
	 * 	@return cost of variable i
	 */
	public def costOnVariable( var i : Int ) : Int
	{
		return err(i);
	}
	
	/**
	 * 	Records a swap
	 * 	@param i1 not used
	 * 	@param i2 not used
	 */
	public def executedSwap(i1 : Int, i2 : Int)
	{
		cost(err);//err );
	}
	
	/**
	 * 	Returns the total cost of the current solution.
	 * 	Also computes errors on constraints for subsequent calls to
	 * 	Cost_On_Variable, Cost_If_Swap and Executed_Swap.
	 * 	@param shouldBeRecorded 0 for no record 1 for record
	 * 	@return cost of solution
	 */
	public def costOfSolution( shouldBeRecorded : Boolean ) : Int {
		return cost((shouldBeRecorded) ? err : null);
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

		x = variables( i1 );		
		variables( i1 ) = variables( i2 );
		variables( i2 ) = x;

		r = cost (null); //NULL );

		variables(i2) = variables(i1);
		variables(i1) = x;

		return r;  
	}
	
	/**
	 * 	Performs a reset
	 * 	@param n not used
	 * 	@param csp problem model
	 * 	@param totalCost
	 * 	@return the new cost or -1 if unknown or some other data are not updated
	 */
	public def reset(n : Int, totalCost : Int) : Int
	{
		var i : Int;
		var j : Int;
		var k : Int;
		var sz : Int;
		var max : Int = 0n;
		var nbMax : Int = 0n;
		var imax : Int;
		var costToExit : Int = totalCost;
		var bestCost : Int = x10.lang.Int.MAX_VALUE;
		var cost : Int;

		Rail.copy( variables , saveSol ); //memcpy(save_sol, sol, size_bytes);

		for(i = 0n; i < length; i++) /* collect most erroneous vars */
		{
			if (err(i) > max)
			{
				max = err(i);
				iErr(0) = i;
				nbMax = 1n;
			}
			else if (err(i) == max)
				iErr(nbMax++) = i;
		}
		
		iErr = r.randomArrayPermut(iErr);
		imax = iErr(--nbMax); /* chose one var random (most often there is only one) - the last and dec nb_max */
		
		/* A way to reset: try to shift left/right all sub-vectors starting or ending by imax
		 * need sol[] to be as at entry.
		 */
		
		// #if 1
		for(k = 0n; k < length; k++)
		{
			/* we need a random here to avoid to be trapped in the same "bests" chain (see best_cost) */
			
			if (r.randomDouble() < 0.4)
				continue;
			
			if (imax < k)
			{
				i = imax;
				j = k;
			}
			else
			{
				i = k;
				j = imax;
			}
			sz = j - i;
			
			if (sz <= 1n)
				continue;
			
			//sz *= sizeof(int);
			
			/* the following test is not precise (could be different),
			 * we only want to avoid to do both left and right shift for efficiency reasons */
			
			if (imax < size2)
			{ /* shift left 1 cell */
				Rail.copy(saveSol, i as Long + 1L, variables, i as Long, sz as Long);//memcpy(sol + i, save_sol + i + 1, sz);
				variables(j) = saveSol(i);
				
				if ((cost = cost(null)) < costToExit)
					return -1n; /* -1 because the err[] is not up-to-date */
				
				if (cost < bestCost || (cost == bestCost && r.randomDouble() < 0.2))
				{
					bestCost = cost;
					Rail.copy(variables, bestSol); //memcpy(best_sol, sol, size_bytes);
				}
			}
			else
			{ /* shift right 1 cell */
				Rail.copy(saveSol, i as Long, variables, i as Long + 1L, sz as Long);//memcpy(sol + i + 1, save_sol + i, sz);
				variables(i) = saveSol(j);
				
				if ((cost = cost(null)) < costToExit)
					return -1n;
				
				if (cost < bestCost || (cost == bestCost && r.randomDouble() < 0.2))
				{
					bestCost = cost;
					Rail.copy(variables, bestSol); //memcpy(best_sol, sol, size_bytes);
				}
			}
			/* restore */
			Rail.copy( saveSol, i as Long, variables, i as Long, sz as Long + 1L); //memcpy(sol + i, save_sol + i, sz + sizeof(int));
		}
		// #endif
		
		
		/* A way to reset: try to add a constant (circularly) to each element.
		 * does not need sol[] to be as entry (uses save_sol[]).
		 */
		
		// #if 1
		for(j = 0n; (k = toAdd(j)) != 0n; j++)
		{
			for(i = 0n; i < length; i++)
				if ((variables(i) = saveSol(i) + k) > length)
					variables(i) -= length;

			if ((cost = cost(null)) < costToExit)
				return -1n; /* -1 because the err[] is not up-to-date */
			
			// #if 1
			if (cost < bestCost && r.randomDouble() < 0.33333333333)
			{
				bestCost = cost;
				Rail.copy( variables, bestSol ); //memcpy(best_sol, sol, size_bytes);
			}
			// #endif
			
		}
		// memcpy(sol, save_sol, size_bytes); // can be needed depending if what follows need inital sol[]
		// #endif
		
		
		
		/* A way to reset: try to shift left from the beginning to some erroneous var.
		 * does not need sol[] to be as entry (uses save_sol[]).
		 */
		
		// #if 1
		
		// #define NB_OF_ERR_VARS_TO_TRY 3
		
		var nbErr : Int = nbMax; /* NB nb_max has been dec (see above) - thus we forget cur "imax" */
		if (nbErr < 3n ) //NB_OF_ERR_VARS_TO_TRY) /* add other erroneous vars in i_err[] */
		{
			for(i = 0n; i < length; i++)
				if (err(i) > 0n && err(i) < max)
					iErr(nbErr++) = i;
			var auxArray : Rail[Int] = new Rail[Int](nbErr - nbMax + 1n);
			Rail.copy(iErr, nbMax as Long, auxArray, 0 , (nbErr - nbMax) as Long );
			auxArray = r.randomArrayPermut(auxArray);
			Rail.copy(auxArray, 0, iErr, nbMax as Long, (nbErr - nbMax) as Long );
			// Random_Array_Permut(i_err + nb_max, nb_err - nb_max); /* some randomness on new vars (don't touch max vars) */
		}

		for(k = 0n; k < 3n; k++) //k < NB_OF_ERR_VARS_TO_TRY
		{
			imax = iErr(k);
			
			if (imax == 0n || /*imax == size - 1 ||*/ r.randomDouble() < 0.33333333333)
				continue;
			
			Rail.copy(saveSol, imax as Long, variables, 0, (length - imax) as Long); //memcpy(sol, save_sol + imax, (size - imax) * sizeof(int));
			Rail.copy(saveSol, 0, variables, (length - imax) as Long, imax as Long); //memcpy(sol + size - imax, save_sol, imax * sizeof(int));
			
			if ((cost = cost(null)) < costToExit) /* only if it is a var with max error */
				return -1n; /* -1 because the err[] is not up-to-date */
			
			if (cost < bestCost)
			{
				bestCost = cost;
				Rail.copy(variables, bestSol); //memcpy(best_sol, sol, size_bytes);
			}
		}
		
		// #endif
		
		
		/* return the best found solution */
		
		Rail.copy(bestSol, variables); //memcpy(sol, best_sol, size_bytes);
		
		return -1n; /* -1 because the err[] is not up-to-date */
	} 

	public  def verify(conf:Valuation(sz)):Boolean {
		var i:Int, j:Int, d:Int;
		var r:Int = 1n;

		//Check Permutation
		val permutV = new Rail[Int](sz, 0n);
		val baseV = solverParams.baseValue;
		for (mi in conf.range()){
			val value = conf(mi);
			permutV(value-baseV)++;
			if (permutV(value-baseV)>1){
				Console.OUT.println("Not valid permutation, value "+ value +" is repeted");
			}
		}
		
		
		for(i = 1n; i < length; i++)
		{
			nbOcc.clear();
			for(j = i; j < length; j++)
			{
				d = conf(j - i) - conf(j);
				nbOcc(d + length) = nbOcc(d + length) + 1n;
			}
			
			for(d = 1n; d < 2n * length; d++)
			{
				var nr:Int = nbOcc(d);
				if (nr > 1)
				{
					var dist:Int = d - length;
					Console.OUT.println("ERROR at row "+i+": distance "+dist+" appears "+nr+" times");
					r = 0n;
				}
			}
		} 
		return r==1n;
	}
}
public type CostasAS(s:Long)=CostasAS{self.sz==s};
