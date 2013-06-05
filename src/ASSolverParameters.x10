/** ASSolverParameters
 * 	Encapsulate all the parameters for AS Solver
 * 
 *  Based on the C implementation of Adaptive Search algoritm by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013 First Version
 */

public class ASSolverParameters{
	/** perform an exhausitve search */ 
	var exhaustive : Boolean;
	/** stop as soon as a better swap is found */
	var firstBest : Boolean;
	/** % to select local min instead of staying on a plateau (or >100 to not use)*/
	var probSelectLocMin : Int;	
	/** nb swaps to freeze a (local min) var */
	var freezeLocMin : Int;
	/** nb swaps to freeze 2 swapped vars */
	var freezeSwap : Int;
	/** nb of frozen vars before reset */
	var resetLimit : Int;
	/** nb variables to reset */
	var nbVarToReset : Int;
	/** nb of iterations before restart */
	var restartLimit : Int;	
	/** max nb of times to restart (to retry) */
	var restartMax : Int;
	/** true if Cost_Of_Solution must be called twice */
	var reinitAfterIfSwap : Int;	
	/** percentage of variables to reset */
	var resetPercent : Int;		

	var baseValue : Int;
	/** Probability to change bad vector for a vector in the pool (Comm Enable) */
	var probChangeVector : Int;
	
	/**
	 * 	Constructor
	 */
	public def this(){
		firstBest = false; //revisar val por default
		nbVarToReset = -1;
		probChangeVector = 100;
	}
	
	/**
	 *  set the values of the parameters to the solver
	 * 	@param toSet parameters to set
	 */
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