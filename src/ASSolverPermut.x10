import x10.util.Random;

/** ASSolverPermut is the implementation of Adaptive Search solver
 * 	in the x10 lenguage.
 *  Implementation specialized in Permuts Problems and no exhaustive search.
 * 
 *  Based on the C implementation of Adaptive Search algoritm by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013
 */

public class ASSolverPermut {

val mark : Array[Int](1); 
val size : Int;  
val solverP : ASSolverParameters; 

var nb_var_to_reset : Int; 
var nb_reset : Int;

var max_i : Int;		//static int max_i ALIGN;		/* swap var 1: max projected cost (err_var[])*/
var min_j : Int;

var best_cost : Int;
var new_cost : Int;
var total_cost : Int;

var list_i_nb : Int;
var list_j_nb : Int;
var list_i : Array[Int](1); 

//Counters
var nb_restart : Int;
var nb_iter : Int;
var nb_var_marked : Int;
var nb_swap : Int;
 
val random : RandomTools;
 
var kill : Boolean;

/** Number of iterations to update kill status */
val updateP : Int;

val varRegion : Region(1);
 
/**
 *  Constructor of the class
 * 	@param sizeOfProblem size of the problem to solve
 *  @seed seed for the randomness in the object.
 * 
 */
public def this( sizeOfProblem : Int , seed : Long, updateI:Int) {
	size = sizeOfProblem;
	varRegion = 0..(size - 1);
	mark = new Array[Int](varRegion,0);
	list_i = new Array[Int](varRegion,0); //Why not distributed?
	solverP = new ASSolverParameters();
	random = new RandomTools(seed);
	nb_var_marked = 0;
	nb_restart = 0;
	updateP = updateI; //Default value 
	kill = false;
}

/**
 *  solve( csp : ModelAS ) : Int
 *  Solve a csp Problem through the Adaptive Search algoritm
 * 	@param csp The model of the problem to solve
 *  @return the final total cost after solving process (If success returns 0)
 */ 
public def solve( csp : ModelAS ) : Int {
	
	var nb_in_plateau:Int; 
	
	csp.setParameters(solverP);
	
	nb_var_to_reset = (((size * solverP.resetPercent) + (100) - 1) / (100));
	csp.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
	
	mark.clear();
	list_i.clear();
	
	nb_restart = 0;
	nb_swap = 0;
	nb_iter = 0;
 	nb_in_plateau = 0;
	
	total_cost = csp.costOfSolution();
	best_cost = total_cost;
	var best_of_best: Int = x10.lang.Int.MAX_VALUE ;
	
	while( total_cost != 0 ){
		if (best_cost < best_of_best)
			best_of_best = best_cost;

		nb_iter++;
  
		if (nb_iter >= solverP.restartLimit){
			if(nb_restart < solverP.restartMax){
				csp.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
				mark.clear();
				nb_restart++;
				nb_iter = 0;
				nb_in_plateau = 0;
				best_cost = total_cost = csp.costOfSolution();
				best_of_best = x10.lang.Int.MAX_VALUE ;
				continue;
			}
			break; 
		}
	
		max_i = selectVarHighCost(csp);
		//Console.OUT.print("max_i= "+max_i);
		min_j = selectVarMinConflict(csp);
		//Console.OUT.println("  min_j= "+min_j);
		
		
		//Console.OUT.println("----- iter no: "+nb_iter+", cost: "+total_cost+", nb marked: "+nb_var_marked+" ---, nb_swap= "+nb_swap);
		
		if (total_cost != new_cost)
		{
			if (nb_in_plateau > 1)
		 	{
		 		//Console.OUT.println("end of plateau, length: "+ nb_in_plateau);
		 	}
		 	nb_in_plateau = 0;
		}
		if (new_cost < best_cost)
			best_cost = new_cost;
		
		nb_in_plateau++;
		
		if (min_j == -1) //What??
			continue;
		
 		if (max_i == min_j)
		{
			mark(max_i) = nb_swap + solverP.freezeLocMin; //Mark(max_i, freeze_loc_min);
 			if (nb_var_marked > solverP.resetLimit)
 			{
 				//Console.OUT.println("\tTOO MANY FROZEN VARS - RESET");
 				doReset(nb_var_to_reset,csp);//doReset(nb_var_to_reset,csp);
 			}
		}
		else
		{
			mark(max_i) = nb_swap + solverP.freezeSwap; //Mark(max_i, ad.freeze_swap);
			mark(min_j) = nb_swap + solverP.freezeSwap; //Mark(min_j, ad.freeze_swap);
		
			csp.swapVariables(max_i, min_j);//adSwap(max_i, min_j,csp);
			csp.executedSwap(max_i, min_j);
			total_cost = new_cost;
		}
 		
 		if( nb_iter % updateP == 0 ){
 			Runtime.probe();
 			if(kill)
 				break;
 		}
	}
	
	if(!kill)
		Console.OUT.print("Iter no: "+nb_iter+"\tcost: "+total_cost+"\t\tnb marked: "+nb_var_marked+"\t"+here);
	
	return total_cost;
}

/**
 * 	selectVarHighCost( csp : ModelAS ) : Int
 * 	Select the maximum cost variable of the problem 
 *  Also computes the number of marked variables.
 *  @param csp problem model
 * 	@return the index of the variable with high individual cost
 */
public def selectVarHighCost( csp : ModelAS ) : Int{
	
	var i: Int;
	var x: Int;
	var max: Int; 

	list_i_nb = 0; //Number of elements
	max = 0;
	nb_var_marked = 0;
	i = -1; 
	//Console.OUT.println("Aqui");
	while(++i < size) 
	{
		if (nb_swap < mark(i))
		{
			nb_var_marked++;
			continue;
		}
		//Console.OUT.println("Aqui");
		x = csp.costOnVariable(i);
		//Console.OUT.println("Aqui");
		if (x >= max){
			if (x > max){
				max = x;
				list_i_nb = 0;
			}
			list_i(list_i_nb++) = i; 
		}
	}
	
	x = random.randomInt(list_i_nb);
	//Console.OUT.println("list_i_nb "+list_i_nb+ " x "+x+" list_i(x) "+list_i(x));
	max_i = list_i(x); //This max_i must be local or only returns the value
	return max_i;
}

/**
 *	selectVarMinConflict( csp : ModelAS) : Int
 *	Computes swap and selects the minimum of cost if swap
 * 	@param csp problem model
 * 	@return the index of the variable with minimum individual cost if swap
 */
public def selectVarMinConflict( csp : ModelAS) : Int {
	var j: Int;
	var x: Int;
	var flagOut:Boolean = false; 
	var lmin_j:Int=min_j;
	
	//loop: 
	do{
		flagOut = false;
		list_j_nb = 0;
 		new_cost = total_cost;
 		//Console.OUT.println("total_cost"+total_cost);
 		j = -1;
 	
	 	while(++j < size) 
	 	{	
	 		//Console.OUT.println("swap "+j+"/"+max_i);
	 		x = csp.costIfSwap(total_cost, j, max_i);
	 		//Console.OUT.println("swap "+j+"/"+max_i+"  Cost= "+x);
	 		
	 		if (solverP.probSelectLocMin <= 100 && j == max_i)
	 			continue;
	 		
	 		//
	 		if (x < new_cost){
	 			list_j_nb = 1;
	 			new_cost = x;
	 			lmin_j = j;
	 			if (solverP.firstBest)
	 			{
	 				return lmin_j;         
	 			}
	 		} else if (x == new_cost){
	 			if (random.randomInt(++list_j_nb) == 0)
	 				lmin_j = j; 
	 		}
	 	}
 	
	 	if (solverP.probSelectLocMin <= 100)
	 	{
	 		if (new_cost >= total_cost && 
	 			(random.randomInt(100) < solverP.probSelectLocMin ||(list_i_nb <= 1 && list_j_nb <= 1)))
	 		{
	 			lmin_j = max_i;
	 			return lmin_j;
	 		}
	
	 		if (list_j_nb == 0)
	 		{
	 			//Console.OUT.println("list_i_nb= "+list_i_nb);
	 			//for(h in list_i)
	 				//Console.OUT.print(" "+list_i(h));
	 			nb_iter++;
	 			x = random.randomInt(list_i_nb);
	 			max_i = list_i(x);
	 			//max_i = list_i(1);
	 			flagOut = true;
	 			//break loop;
	 		}
	 	}
	}while(flagOut);
 	
	return lmin_j;
}

/**
 * 	doReset( var n : Int, csp : ModelAS )
 * 	Performs the reset over the problem model csp
 *  @param n number of variables to reset
 * 	@param csp Model to reset
 */
public def doReset( var n : Int, csp : ModelAS ) {
	var cost:Int = -1;//reset(n, csp);
	
	cost = csp.reset( n );
	
	nb_swap += n ;
	
	mark.clear();
	//nbreset++;
	total_cost = (cost < 0) ? csp.costOfSolution() : cost; //Arg costofsol(1)
}

/**
 * 	Clear function
 */
public def clear(){
}


public def testSelectVarHighCost(csp: ModelAS){ 
	var test:Int;
	
	csp.setParameters(solverP);
	csp.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
	
	mark.clear();
	csp.costOfSolution();
	var timeStart :Long = x10.lang.System.nanoTime();
	test = selectVarHighCost(csp);
	var timeEnd :Long = x10.lang.System.nanoTime(); 
	
	Console.OUT.println("max_i= "+test);
	
	return timeEnd-timeStart;
}

}//End ASSolverPermut Class
	

