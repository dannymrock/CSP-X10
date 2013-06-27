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
	
	var size2:Int;
	var sizeSq:Int;
	/** nb occurrences of each diff (translated) */
	val nbOcc : Rail[Int];		/* diff are in -(size-1)..-1 1..size-1 */
								/* translated are in 0..2*size-1 [0] and [N] being unused */						
	/** records the indice of a first occurence of a (translated) difference */
	val first : Rail[Int];
	/** errors on variables */
	val err : Rail[Int];
	
	/* for reset: */
	/** save the sol[] vector */
	val saveSol : Rail[Int];
	/** save the best sol[] found in a reset phase */
	val bestSol : Rail[Int];
	/** indices of erroneous vars */
	var iErr : Rail[Int];			
	
	val toAdd : Rail[Int];
	
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
		
		nbOcc = new Rail[Int]( length2reg , 0);
		first =  new Rail[Int]( length2reg , 0);
		err =  new Rail[Int]( varRegion , 0);
		saveSol = new Rail[Int]( varRegion , 0);
		bestSol = new Rail[Int]( varRegion , 0);
		iErr = new Rail[Int]( varRegion , 0);
		
		toAdd = new Rail[Int]( 0..9 );
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
		
		solverParams.probChangeVector = 1;
		
		toAdd(0) = 1; toAdd(1) = 2; toAdd(2) = length - 2; toAdd(3) = length - 3;
		
	} 
	
	/**
	 * 	Computes the cost of the solution
	 * 	@param err vector of error on variables (if null don't update object err vector)
	 * 	@return cost
	 */
	def cost( err : Rail[Int] ) : Int {		//int *err)	 //Inline
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
			var penalty : Int = sizeSq - (dist * dist);
			//var penalty : Int = 1;
			//var penalty : Int = sizeSq - dist;
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
}
