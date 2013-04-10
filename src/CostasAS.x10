/** CostasAS is the implementation of Costas Array problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  This code is an adaptation in x10 of the C implementation of Adaptive Search algoritm 
 * 	by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 10, 2013 First Version
 */

public class CostasAS extends ModelAS{
	
	var size2:Int;
	var sizeSq:Int;
	/** nb occurrences of each diff (translated) */
	val nbOcc : Array[Int];		/* diff are in -(size-1)..-1 1..size-1 */
								/* translated are in 0..2*size-1 [0] and [N] being unused */						
	/** records the indice of a first occurence of a (translated) difference */
	val first : Array[Int];
	/** errors on variables */
	val err : Array[Int];
	
	/* for reset: */
	/** save the sol[] vector */
	val saveSol : Array[Int];
	/** save the best sol[] found in a reset phase */
	val bestSol : Array[Int];
	/** indices of erroneous vars */
	var iErr : Array[Int];			
	
	val toAdd : Array[Int];
	
	val length2reg : Region(1);
	
	/**
	 * 	Constructor
	 *  @param lengthProblem Number of variables of the problem
	 * 	@param seed Desired seed for randomness of the problem
	 */
	def this (val lengthProblem : Int, seed : Long){
		super(lengthProblem, seed);
		size2 = (length - 1) / 2;		
		sizeSq = length * length;
		length2reg = 0..((length * 2)-1);
		
		nbOcc = new Array[Int]( length2reg , 0);
		first =  new Array[Int]( length2reg , 0);
		err =  new Array[Int]( varRegion , 0);
		saveSol = new Array[Int]( varRegion , 0);
		bestSol = new Array[Int]( varRegion , 0);
		iErr = new Array[Int]( varRegion , 0);
		
		toAdd = new Array[Int]( 0..9 );
		initParameters();
	}

	/**
	 * 	initParameters() 
	 *  Set Initial values for the problem
	 */
	private def initParameters(){
		solverParams.probSelectLocMin = 50;
		solverParams.freezeLocMin = 1;
		solverParams.freezeSwap = 0;
		solverParams.resetLimit = 1;
		solverParams.resetPercent = 5;
		solverParams.restartLimit = 1000000000;
		solverParams.restartMax = 0;
		solverParams.baseValue = 1;
		solverParams.exhaustive = false;
		solverParams.firstBest = false;
		
		toAdd(0) = 1; toAdd(1) = 2; toAdd(2) = length - 2; toAdd(3) = length - 3;
		
	} 
	
	/**
	 * 	Computes the cost of the solution
	 * 	@param err vector of error on variables (if null don't update object err vector)
	 * 	@return cost
	 */
	def cost( err : Array[Int] ) : Int {		//int *err)	 //Inline
		var dist : Int = 1;
		var i : Int;
		var firstI : Int;
		var diff : Int;
		var diffTranslated : Int;
		var nb : Int;
		var r : Int = 0;

		if (err != null) 
			err.clear();	//memset(err, 0, size * sizeof(int));

		do
		{
			nbOcc.clear();
			i = dist;
			do
			{
				diff = variables( i - dist ) - variables(i);
				diffTranslated = diff + length;
				nbOcc(diffTranslated) = nbOcc(diffTranslated) + 1;
				nb = nbOcc(diffTranslated);

				if (err != null) //if (err) ????
				{
					if (nb == 1) 
						first(diffTranslated) = i;
					else
					{
						if (nb == 2)
						{
							firstI = first(diffTranslated);
							//ErrOn(first_i);
							err(firstI) += (sizeSq - (dist * dist));
							err(firstI - dist) += (sizeSq - (dist * dist)); 
						}
						//ErrOn(i);
						err(i) += (sizeSq - (dist * dist));
						err(i - dist) += (sizeSq - (dist * dist));
					}  
				}

				if (nb > 1)
					r += (sizeSq - (dist * dist));
			}while(++i < length);
		}while(++dist <= size2);
		return r;
	}
	/**
	 *  Evaluates the error on a variable.
	 * 	@param i variable
	 * 	@return cost of variable i
	 */
	public def CostOnVariable( i : Int ) : Int
	{
		return err(i);
	}
	
	/**
	 * 	Records a swap
	 * 	@param i1 not used
	 * 	@param i2 not used
	 */
	public def executedSwap(i1:Int, i2:Int)
	{
		cost(err);//err );
	}
	
	/**
	 *	Returns the total cost of the current solution.
	 *	Also computes errors on constraints for subsequent calls to
	 *	Cost_On_Variable, Cost_If_Swap and Executed_Swap.
	 * 	@param shouldBeRecorded 0 for no record 1 for record
	 * 	@return cost of solution
	 */
	public def costOfSolution( shouldBeRecorded : Int ) : Int {
		return cost((shouldBeRecorded != 0) ? err : null);
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
	 *	Performs a reset
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
 		var max : Int = 0;
 		var nbMax : Int = 0;
 		var imax : Int;
 		var costToExit : Int = totalCost;
 		var bestCost : Int = x10.lang.Int.MAX_VALUE;
 		var cost : Int;

 		Array.copy( variables , saveSol );  //memcpy(save_sol, sol, size_bytes);

 		for(i = 0; i < length; i++)	/* collect most erroneous vars */
 		{
 			if (err(i) > max)
 			{
 				max = err(i);
 				iErr(0) = i;
				nbMax = 1;
 			} 
 			else if (err(i) == max)
 				iErr(nbMax++) = i;
 		}
 
 		iErr = r.randomArrayPermut(iErr);
 		imax = iErr(--nbMax); /* chose one var random (most often there is only one) - the last and dec nb_max */
 
 		/* A way to reset: try to shift left/right all sub-vectors starting or ending by imax
 		 *                 need sol[] to be as at entry.
 		 */
 
// 		#if 1
 		for(k = 0; k < length; k++)
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
 
 			if (sz <= 1)
 				continue;
 
 			//sz *= sizeof(int);
 
 			/* the following test is not precise (could be different),
 			 * we only want to avoid to do both left and right shift for efficiency reasons */
 
 			if (imax < size2)	
 			{			/* shift left 1 cell */
 				Array.copy(saveSol, i+1, variables, i, sz );//memcpy(sol + i, save_sol + i + 1, sz);
 				variables(j) = saveSol(i);
 
 				if ((cost = cost(null)) < costToExit)
 					return -1;		/* -1 because the err[] is not up-to-date */
 
 				if (cost < bestCost || (cost == bestCost && r.randomDouble() < 0.2))
 				{
 					bestCost = cost;
 					Array.copy( variables, bestSol ); //memcpy(best_sol, sol, size_bytes);
 				}
 			} 
 			else 
 			{			/* shift right 1 cell */
 				Array.copy(saveSol, i, variables, i+1, sz);//memcpy(sol + i + 1, save_sol + i, sz);
 				variables(i) = saveSol(j);
 
 				if ((cost = cost(null)) < costToExit)
 					return -1;
 
 				if (cost < bestCost || (cost == bestCost && r.randomDouble() < 0.2))
 				{
 					bestCost = cost;
 					Array.copy(variables, bestSol); //memcpy(best_sol, sol, size_bytes);
 				}
 			}
 			/* restore */
 			Array.copy( saveSol, i, variables, i, sz+1); //memcpy(sol + i, save_sol + i, sz + sizeof(int));      
		}
// 		#endif
 
 
 		/* A way to reset: try to add a constant (circularly) to each element.
 		 *                 does not need sol[] to be as entry (uses save_sol[]). 
 		 */
 
// 		#if 1
 		for(j = 0; (k = toAdd(j)) != 0; j++)
 		{
 			for(i = 0; i < length; i++)
 				if ((variables(i) = saveSol(i) + k) > length)
 					variables(i) -= length;

 			if ((cost = cost(null)) < costToExit)
 				return -1;      /* -1 because the err[] is not up-to-date */
 
// 			#if 1
			if (cost < bestCost && r.randomDouble() < 0.33333333333)
 			{
 				bestCost = cost;
 				Array.copy( variables, bestSol ); //memcpy(best_sol, sol, size_bytes);
			}
// 			#endif
 
 		}
 		//  memcpy(sol, save_sol, size_bytes); // can be needed depending if what follows need inital sol[] 
// 		#endif
 
 
 
 		/* A way to reset: try to shift left from the beginning to some erroneous var.
 		 *                 does not need sol[] to be as entry (uses save_sol[]). 
 		 */
 
// 		#if 1
 
// 		#define NB_OF_ERR_VARS_TO_TRY 3
 
 		var nbErr : Int = nbMax;		/* NB nb_max has been dec (see above) - thus we forget cur "imax" */
 		if (nbErr < 3 )	//NB_OF_ERR_VARS_TO_TRY)		/* add other erroneous vars in i_err[] */
 		{
 			for(i = 0; i < length; i++)
 				if (err(i) > 0 && err(i) < max)
 					iErr(nbErr++) = i;
 			var auxArray : Array[Int] = new Array[Int](0..(nbErr - nbMax));
 			Array.copy(iErr, nbMax, auxArray, 0 , nbErr - nbMax );
 			auxArray = r.randomArrayPermut(auxArray);
 			Array.copy(auxArray, 0, iErr, nbMax, nbErr - nbMax );
// 			Random_Array_Permut(i_err + nb_max, nb_err - nb_max); /* some randomness on new vars (don't touch max vars) */
 		}

 		for(k = 0; k < 3; k++) //k < NB_OF_ERR_VARS_TO_TRY
 		{
 			imax = iErr(k);
 
 			if (imax == 0 || /*imax == size - 1 ||*/ r.randomDouble() < 0.33333333333)
 				continue;
 
 			Array.copy(saveSol, imax, variables, 0, length - imax);	//memcpy(sol, save_sol + imax, (size - imax) * sizeof(int));
 			Array.copy(saveSol, 0, variables, length - imax, imax);	//memcpy(sol + size - imax, save_sol, imax * sizeof(int));
 
 			if ((cost = cost(null)) < costToExit) /* only if it is a var with max error */
 				return -1;      /* -1 because the err[] is not up-to-date */
 
 			if (cost < bestCost)
 			{
 				bestCost = cost;
 				Array.copy(variables, bestSol);	//memcpy(best_sol, sol, size_bytes);
			}
 		}
 
// 		#endif
 
 
 		/* return the best found solution */
 
 		Array.copy(bestSol, variables);	//memcpy(sol, best_sol, size_bytes);
 
 		return -1;      /* -1 because the err[] is not up-to-date */
 	}	
}