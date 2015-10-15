package csp.solver;
import csp.model.ICOPModel;
/**
 * A Solver runs a meta-heuristic algorithms, within the frame of a
 * ISolver instance. 
 * 
 * <p> Therefore to design a new meta-heuristic solver, simply have it implement
 * ISolver.
 * 
 */
public interface ISolver {
	 property sz():Long;
	 
	 /**
	  * Solves the problem, which is specified by a Combinatorial
	  * Optimization Problem.
	  */
	 def solve( cop : ICOPModel{self.sz==this.sz}, tCost : Long, sLow: Boolean) : Long;
	 
	 
	 /**
	  *  Set the seed used for the random number generator.
	  */
	 def setSeed(seed:Long):void;
	  
	 
	 /**
	  * 	Clean solver variables to prepare a new solver execution.
	  */
	 def clear():void;
	 
	 /**
	  * 	Return the Configuration with the best cost
	  */
	 def getBestConfiguration():Valuation(sz);
	 
	 /**
	  * 	Return the cost of the best Configuration
	  */
	 def getBestCost():Long;
	 
	 /**
	  * 	Stop the current search process
	  */
	 def kill():void;
	 
	 /**
	  * 	Report statistics from the solving process
	  */
	 def reportStats( c : CSPStats ):void;
	 
	 /**
	  * 	Re-initialize all variables when restart
	  */
	 def forceRestart():void;
	 
	 def forceReset():void;
}
public type ISolver(s:Long) = ISolver{self.sz==s};