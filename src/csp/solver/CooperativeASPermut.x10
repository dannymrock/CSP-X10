package csp.solver;


import csp.models.*;
import csp.utils.*;

/** ASSolverPermut is the implementation of Adaptive Search solver
 * 	in the x10 lenguage.
 *  Implementation specialized in Permuts Problems.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013 -> first version
 * 				 April 12, 2013 -> Exahustive search implemented
 * 	
 */

import x10.util.Random;
public class CooperativeASPermut extends ASPermut{

	
	//var kill : Boolean;
	var forceRestart : Boolean;
	
	var nbInterTComm: Int;
	var nbForceRestart : Int;
	
	val myID : Int;
	
	/**
	 *  Constructor of the class
	 * 	@param sizeOfProblem size of the problem to solve
	 *  @seed seed for the randomness in the object.
	 * 
	 */
	public def this( aID : Int , sizeOfProblem : Int , seed : Long, conf : ASSolverConf) {
		super(sizeOfProblem , seed, conf);
		myID = aID; 
		//kill = false;
		forceRestart = false; 
		nbInterTComm = 0n;
		nbForceRestart = 0n;
	}
	
	/* To Fix in solve function:
	 * Clear SM at restart: Team.control.clear();
	 * Keep in mind: Reset function
	 * 
	 *
	 */
	
	/**
	 * 	Kill event if ther is interaction with other solvers
	 */
	public def kill():Boolean{
		Runtime.probe();		// Give a chance to the other activities
		// Shared Memory implementation with static object (To change)
		return Team.control.exit;				// Check if other place or activity have finished
	}
	
	/**
	 *  Interaction with other components in the execution 
	 */
	public def interact(csp:ModelAS){
		
		// if( nbIter % solverC.intraTI == 0n ){
		// 	//Console.OUT.println("In ");
		// 	//Chang//
		// 	val res = solverC.communicate( total_cost, csp.variables); 
		// 	if (random.randomInt(100n) < solverP.probChangeVector){
		// 		val result = solverC.getIPVector(csp, total_cost );
		// 		if (result != -1n){
		// 			nbChangeV++;
		// 			nbSwap += size ; //I don't know what happened here with costas reset
		// 			mark.clear();
		// 			total_cost = csp.costOfSolution(1n);
		// 			//Console.OUT.println("Changing vector in "+ here);
		// 		}
		// 		
		// 	}	
		// 	//Console.OUT.println("Print Vectors("+here.id+") :");
		// 	//myComm.printVectors();
		// 	//Main.show("Vector ",csp.variables);
		// 	
		// }
	}
	
	
}//End CooperativeASPermut Class
