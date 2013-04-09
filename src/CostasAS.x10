public class CostasAS extends ModelAS{
	
	var size2:Int;
	var sizeSq:Int;
	 
	val nbOcc : Array[Int];		/* nb occurrences of each diff (translated) */
									/* diff are in -(size-1)..-1 1..size-1 */
									/* translated are in 0..2*size-1 [0] and [N] being unused */
	val first : Array[Int];			/* records the indice of a first occurence of a (translated) difference */
	val err : Array[Int];			/* errors on variables */
	
		/* for reset: */
	val saveSol : Array[Int];		/* save the sol[] vector */
	val bestSol : Array[Int];		/* save the best sol[] found in a reset phase */
	val iErr : Array[Int];			/* indices of erroneous vars */
	
	val length2reg : Region(1);
	
	def this (val lengthProblem:Int, seed:Long){
		super(lengthProblem*lengthProblem, seed);
		size2 = (length - 1) / 2;		
		sizeSq = length * length;
		length2reg = 0..((length * 2)-1);
		
		nbOcc = new Array[Int]( length2reg , 0);
		first =  new Array[Int]( length2reg , 0);
		err =  new Array[Int]( varRegion , 0);
		saveSol = new Array[Int]( varRegion , 0);
		bestSol = new Array[Int]( varRegion , 0);
		iErr = new Array[Int]( varRegion , 0);
	}
	
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
		
	} 
	
	def cost():Int {		//int *err)	 //Inline
		var dist : Int = 1;
		var i : Int;
		var firstI : Int;
		var diff : Int;
		var diffTranslated : Int;
		var nb : Int;
		var r : Int = 0;

		//if (err) 
			//memset(err, 0, size * sizeof(int));

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

				if (true)//if (err) ????
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

	public def CostOnVariable(i : Int) : Int
	{
		return err(i);
	}
	
	public def executedSwap(i1:Int, i2:Int)
	{
		cost( );//err );
	}
	
	public def costOfSolution():Int//int should_be_recorded)
	{
		return cost();//(should_be_recorded) ? err : NULL);
	}
	
	public def costIfSwap(current_cost:Int, i1:Int, i2:Int):Int
	{
		var x : Int;
		var r : Int;

		x = variables( i1 );
		variables( i1 ) = variables( i2 );
		variables( i2 ) = x;

		r = cost ( ); //NULL );

		variables(i2) = variables(i1);
		variables(i1) = x;

		return r;  
	}
	
	
// 	public def reset(n : Int, csp:ModelAS)
// 	{
// 		int i, j, k, sz;
// 		int max = 0, nb_max = 0, imax;
// 		int cost_to_exit = p_ad->total_cost;
// 		int best_cost = BIG;
// 		int cost;
// 
// 		memcpy(save_sol, sol, size_bytes);
// 
// 		for(i = 0; i < size; i++)	/* collect most erroneous vars */
// 		{
// 			if (err[i] > max)
// 			{
// 				max = err[i];
// 				i_err[0] = i;
// 				nb_max = 1;
// 			} 
// 			else if (err[i] == max)
// 				i_err[nb_max++] = i;
// 		}
// 
// 		Random_Array_Permut(i_err, nb_max);
// 		imax = i_err[--nb_max]; /* chose one var random (most often there is only one) - the last and dec nb_max */
// 
// 
// 		/* A way to reset: try to shift left/right all sub-vectors starting or ending by imax
// 		 *                 need sol[] to be as at entry.
// 		 */
// 
// 		#if 1
// 		for(k = 0; k < size; k++)
// 		{
// 			/* we need a random here to avoid to be trapped in the same "bests" chain (see best_cost) */
// 
// 			if (Random_Double() < 0.4)
// 				continue;
// 
// 			if (imax < k)
// 			{
// 				i = imax;
// 				j = k;
// 			}
// 			else
// 			{
// 				i = k;
// 				j = imax;
// 			}
// 			sz = j - i;
// 
// 			if (sz <= 1)
// 				continue;
// 
// 			sz *= sizeof(int);
// 
// 			/* the following test is not precise (could be different),
// 			 * we only want to avoid to do both left and right shift for efficiency reasons */
// 
// 			if (imax < size2)	
// 			{			/* shift left 1 cell */
// 				memcpy(sol + i, save_sol + i + 1, sz);
// 				sol[j] = save_sol[i];
// 
// 				if ((cost = Cost(NULL)) < cost_to_exit)
// 					return -1;		/* -1 because the err[] is not up-to-date */
// 
// 				if (cost < best_cost || (cost == best_cost && Random_Double() < 0.2))
// 				{
// 					best_cost = cost;
// 					memcpy(best_sol, sol, size_bytes);
// 				}
// 			} 
// 			else 
// 			{			/* shift right 1 cell */
// 				memcpy(sol + i + 1, save_sol + i, sz);
// 				sol[i] = save_sol[j];
// 
// 				if ((cost = Cost(NULL)) < cost_to_exit)
// 					return -1;
// 
// 				if (cost < best_cost || (cost == best_cost && Random_Double() < 0.2))
// 				{
// 					best_cost = cost;
// 					memcpy(best_sol, sol, size_bytes);
// 				}
// 			}
// 			/* restore */
// 			memcpy(sol + i, save_sol + i, sz + sizeof(int));      
// 		}
// 		#endif
// 
// 
// 		/* A way to reset: try to add a constant (circularly) to each element.
// 		 *                 does not need sol[] to be as entry (uses save_sol[]). 
// 		 */
// 
// 		#if 1
// 		for(j = 0; (k = to_add[j]) != 0; j++)
// 		{
// 			for(i = 0; i < size; i++)
// 				if ((sol[i] = save_sol[i] + k) > size)
// 					sol[i] -= size;
// 
// 			if ((cost = Cost(NULL)) < cost_to_exit)
// 				return -1;      /* -1 because the err[] is not up-to-date */
// 
// 			#if 1
// 			if (cost < best_cost && Random_Double() < 0.33333333333)
// 			{
// 				best_cost = cost;
// 				memcpy(best_sol, sol, size_bytes);
// 			}
// 			#endif
// 
// 		}
// 		//  memcpy(sol, save_sol, size_bytes); // can be needed depending if what follows need inital sol[] 
// 		#endif
// 
// 
// 
// 		/* A way to reset: try to shift left from the beginning to some erroneous var.
// 		 *                 does not need sol[] to be as entry (uses save_sol[]). 
// 		 */
// 
// 		#if 1
// 
// 		#define NB_OF_ERR_VARS_TO_TRY 3
// 
// 		int nb_err = nb_max;		/* NB nb_max has been dec (see above) - thus we forget cur "imax" */
// 		if (nb_err < NB_OF_ERR_VARS_TO_TRY)		/* add other erroneous vars in i_err[] */
// 		{
// 			for(i = 0; i < size; i++)
// 				if (err[i] > 0 && err[i] < max)
// 					i_err[nb_err++] = i;
// 			
// 			Random_Array_Permut(i_err + nb_max, nb_err - nb_max); /* some randomness on new vars (don't touch max vars) */
// 		}
// 
// 		for(k = 0; k < NB_OF_ERR_VARS_TO_TRY; k++)
// 		{
// 			imax = i_err[k];
// 
// 			if (imax == 0 || /*imax == size - 1 ||*/ Random_Double() < 0.33333333333)
// 				continue;
// 
// 			memcpy(sol, save_sol + imax, (size - imax) * sizeof(int));
// 			memcpy(sol + size - imax, save_sol, imax * sizeof(int));
// 
// 			if ((cost = Cost(NULL)) < cost_to_exit) /* only if it is a var with max error */
// 				return -1;      /* -1 because the err[] is not up-to-date */
// 
// 			if (cost < best_cost)
// 			{
// 				best_cost = cost;
// 				memcpy(best_sol, sol, size_bytes);
// 			}
// 		}
// 
// 		#endif
// 
// 
// 		/* return the best found solution */
// 
// 		memcpy(sol, best_sol, size_bytes);
// 
// 		return -1;      /* -1 because the err[] is not up-to-date */
// 	}

	
	/***********************/
	
}