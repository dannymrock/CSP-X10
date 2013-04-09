public class ASSolverParameters {
	 
	var exhaustive : Boolean; 			/* perform an exhausitve search */
	var firstBest : Boolean;				/* stop as soon as a better swap is found */
	var probSelectLocMin : Int;	/* % to select local min instead of staying on a plateau (or >100 to not use)*/
	var freezeLocMin : Int;			/* nb swaps to freeze a (local min) var */
	var freezeSwap : Int;			/* nb swaps to freeze 2 swapped vars */
	var resetLimit : Int;			/* nb of frozen vars before reset */
	var nbVarToReset : Int;		/* nb variables to reset */
	var restartLimit : Int;			/* nb of iterations before restart */
	var restartMax : Int;			/* max nb of times to restart (to retry) */
	var reinitAfterIfSwap : Int;	/* true if Cost_Of_Solution must be called twice */
	
	var resetPercent : Int;		/* percentage of variables to reset */
	
	var baseValue : Int; 
	
	// prob_select_loc_min
	// freeze_loc_min
	// freeze_swap
	//var reset_limit:Int; /* nb of frozen vars before reset */
	// reset_percent
	// restart_limit
	// restart_max
	
	
	
	//val mark:Array[Int](1);		//Array for mark variables in taboo fashion
	//val size:Int; 			//Size of the problemto solve
	
	public def this(){
		//size = sizeOfProblem;
		//mark = new Array[Int](1..size,0);
		firstBest = false; //revisar val por default
	}
	
	public def setValues(val toSet: ASSolverParameters){
		
		this.exhaustive = toSet.exhaustive;
		this.firstBest = toSet.firstBest;
		this.probSelectLocMin = toSet.probSelectLocMin;
		this.freezeLocMin = toSet.freezeLocMin;
		this.freezeSwap = toSet.freezeSwap;
		this.resetLimit = toSet.resetLimit;
		this.nbVarToReset = toSet.nbVarToReset;
		this.restartLimit = toSet.restartLimit;
		this.restartMax = toSet.restartMax;
		this.reinitAfterIfSwap = toSet.reinitAfterIfSwap;	
		this.resetPercent = toSet.resetPercent;
		this.baseValue = toSet.baseValue;
	}	
}