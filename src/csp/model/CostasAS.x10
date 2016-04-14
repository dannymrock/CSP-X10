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

public class CostasAS extends GenericModel{  
	
	var halfSize:Long;
	var sizeSq:Long;
	/** nb occurrences of each diff (translated) */
	val nbOcc : Rail[Long];		/* diff are in -(size-1)..-1 1..size-1 */
	/* translated are in 0..2*size-1 [0] and [N] being unused */						
	/** records the indice of a first occurence of a (translated) difference */
	val first : Rail[Long];
	/** errors on variables */
	val err = new Rail[Long] (sz , 0); 
	
	/* for reset: */
	/** save the sol[] vector */
	val saveSol = new Rail[Int] (sz, 0n);
	/** save the best sol[] found in a reset phase */
	val bestSol = new Rail[Int] (sz, 0n);
	/** indices of erroneous vars */
	var iErr:Rail[Int]{self.size==sz,self!=null} = new Rail[Int] (sz, 0n);			
	
	val toAdd : Rail[Long];
	
	//val doubleSizereg : Region(1);
	
	val doubleSize : Long;
	/**
	 * 	Constructor
	 *  @param sizeProblem Number of variables of the problem
	 * 	@param seed Desired seed for randomness of the problem
	 */
	def this (sizeProblem : Long, seed : Long, opts:ParamManager) : CostasAS(sizeProblem) {
		super(sizeProblem, seed, opts);
		
		halfSize = (sizeProblem - 1n) / 2n;		
		sizeSq = sizeProblem * sizeProblem;
		doubleSize = sizeProblem * 2n;
		
		nbOcc = new Rail[Long] (doubleSize , 0);
		first =  new Rail[Long] (doubleSize , 0);
		toAdd = new Rail[Long](10);
		Logger.info(()=>{"Starting CAP"});
		
		toAdd(0) = 1; toAdd(1) = 2; toAdd(2) = size - 2; toAdd(3) = size - 3;
	}

	// 	solverParams.probSelectLocMin = 50n;
	// 	solverParams.freezeLocMin = 1n;
	// 	solverParams.freezeSwap = 0n;
	// 	solverParams.resetLimit = 1n;
	// 	//solverParams.resetLimit = 2n;
	// 	solverParams.resetPercent = 5n;
	// 	solverParams.restartLimit = rLimit;
	// 	solverParams.restartMax = 0n;
	// 	solverParams.baseValue = 1n;
	// 	solverParams.exhaustive = false;
	// 	solverParams.firstBest = false;

	/**
	 * 	Computes the cost of the solution
	 * 	@param err vector of error on variables (if null don't update object err vector)
	 * 	@return cost
	 */
	def cost( err : Rail[Long] ) : Long {		//int *err)	 //Inline
		var dist : Long = 1;
		var i : Long;
		var firstI : Long;
		var diff : Long;
		var diffTranslated : Long;
		var nb : Long;
		var r : Long = 0;

		if (err != null) 
			err.clear();	//memset(err, 0, size * sizeof(int));

		do
		{
			nbOcc.clear();
			i = dist;
			val penalty  = (sizeSq - (dist * dist));
			//var penalty : Int = 1;
			//var penalty : Int = sizeSq - dist;
			do
			{
				diff = variables( i - dist ) - variables(i);
				diffTranslated = diff + size;
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
			}while(++i < size);
		}while(++dist <= halfSize);
		return r;
	}
	/**
	 *  Evaluates the error on a variable.
	 * 	@param i variable
	 * 	@return cost of variable i
	 */
	public def costOnVariable( i:Long ) : Long
	{
		return err(i);
	}
	
	/**
	 * 	Records a swap
	 * 	@param i1 not used
	 * 	@param i2 not used
	 */
	public def executedSwap(i1:Long, i2:Long)
	{
		cost(err);
	}
	
	/**
	 * 	Returns the total cost of the current solution.
	 * 	Also computes errors on constraints for subsequent calls to
	 * 	Cost_On_Variable, Cost_If_Swap and Executed_Swap.
	 * 	@param shouldBeRecorded 0 for no record 1 for record
	 * 	@return cost of solution
	 */
	public def costOfSolution( shouldBeRecorded : Boolean ) : Long {
		return cost((shouldBeRecorded) ? err : null);
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
		var x : Int;
		var r : Long;

		x = variables( i1 );		
		variables( i1 ) = variables( i2 );
		variables( i2 ) = x;

		r = cost(null); //NULL );

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
	public def reset( n:Long, totalCost:Int ) : Long
	{
		var i : Long;
		var j : Long;
		var k : Long;
		var sz : Long;
		var max : Long = 0;
		var nbMax : Long = 0;
		var imax : Long;
		var costToExit : Long = totalCost;
		var bestCost : Long = Long.MAX_VALUE;
		var cost : Long;

		Rail.copy( variables , saveSol ); //memcpy(save_sol, sol, size_bytes);

		for(i = 0; i < size; i++) /* collect most erroneous vars */
		{
			if (err(i) > max)
			{
				max = err(i);
				iErr(0) = i as Int;
				nbMax = 1n;
			}
			else if (err(i) == max)
				iErr(nbMax++) = i as Int;
		}
		
		iErr = this.randomArrayPermut(iErr);
		imax = iErr(--nbMax); /* chose one var random (most often there is only one) - the last and dec nb_max */
		
		/* A way to reset: try to shift left/right all sub-vectors starting or ending by imax
		 * need sol[] to be as at entry.
		 */
		
		// #if 1
		for(k = 0n; k < size; k++)
		{
			/* we need a random here to avoid to be trapped in the same "bests" chain (see best_cost) */
			
			if (r.nextDouble() < 0.4)
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
			
			if (imax < halfSize)
			{ /* shift left 1 cell */
				Rail.copy(saveSol, i as Long + 1L, variables, i as Long, sz as Long);//memcpy(sol + i, save_sol + i + 1, sz);
				variables(j) = saveSol(i);
				
				if ((cost = cost(null)) < costToExit)
					return -1n; /* -1 because the err[] is not up-to-date */
				
				if (cost < bestCost || (cost == bestCost && r.nextDouble() < 0.2))
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
				
				if (cost < bestCost || (cost == bestCost && r.nextDouble() < 0.2))
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
		for(j = 0; (k = toAdd(j)) != 0; j++)
		{
			for(i = 0; i < size; i++)
				if ((variables(i) = (saveSol(i) + k) as Int) > size)
					variables(i) -= size as Int;

			if ((cost = cost(null)) < costToExit)
				return -1n; /* -1 because the err[] is not up-to-date */
			
			// #if 1
			if (cost < bestCost && r.nextDouble() < 0.33333333333)
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
		
		var nbErr : Long = nbMax; /* NB nb_max has been dec (see above) - thus we forget cur "imax" */
		if (nbErr < 3 ) //NB_OF_ERR_VARS_TO_TRY) /* add other erroneous vars in i_err[] */
		{
			for(i = 0n; i < size; i++)
				if (err(i) > 0n && err(i) < max)
					iErr(nbErr++) = i as Int;
			var auxArray : Rail[Int] = new Rail[Int](nbErr - nbMax + 1n);
			Rail.copy(iErr, nbMax as Long, auxArray, 0 , (nbErr - nbMax) as Long );
			auxArray = this.randomArrayPermut(auxArray);
			Rail.copy(auxArray, 0, iErr, nbMax as Long, (nbErr - nbMax) as Long );
			// Random_Array_Permut(i_err + nb_max, nb_err - nb_max); /* some randomness on new vars (don't touch max vars) */
		}

		for(k = 0n; k < 3n; k++) //k < NB_OF_ERR_VARS_TO_TRY
		{
			imax = iErr(k);
			
			if (imax == 0 || /*imax == size - 1 ||*/ r.nextDouble() < 0.33333333333)
				continue;
			
			Rail.copy(saveSol, imax as Long, variables, 0, (size - imax) as Long); //memcpy(sol, save_sol + imax, (size - imax) * sizeof(int));
			Rail.copy(saveSol, 0, variables, (size - imax) as Long, imax as Long); //memcpy(sol + size - imax, save_sol, imax * sizeof(int));
			
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
		val baseV = this.baseValue;
		for (mi in conf.range()){
			val value = conf(mi);
			permutV(value-baseV)++;
			if (permutV(value-baseV)>1){
				Console.OUT.println("Not valid permutation, value "+ value +" is repeted");
			}
		}
		
		
		for(i = 1n; i < size; i++)
		{
			nbOcc.clear();
			for(j = i; j < size; j++)
			{
				d = conf(j - i) - conf(j);
				nbOcc(d + size) = nbOcc(d + size) + 1n;
			}
			
			for(d = 1n; d < 2n * size; d++)
			{
				var nr:Long = nbOcc(d);
				if (nr > 1)
				{
					var dist:Long = d - size;
					val vali =i; val valdist=dist; val valnr=nr;
					Logger.debug(()=>{" ERROR at row "+vali+": distance "+valdist+" appears "+valnr+" times"});
					r = 0n;
				}
			}
		} 
		return r==1n;
	}
	
	public def randomArrayPermut( vec : Rail[Int] ) : Rail[Int]{self==vec}{
		 
		 for(var i:Long = vec.size - 1; i > 0 ; i--)
		 {
			  val j = r.nextLong(i + 1);
			  val z = vec(i);
			  vec(i) = vec(j);
			  vec(j) = z;
		 }
		 return vec;	
	}
	
}
public type CostasAS(s:Long)=CostasAS{self.sz==s};
