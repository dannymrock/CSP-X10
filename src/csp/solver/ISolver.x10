package csp.solver;
import csp.model.ModelAS;
/**
 * A solver runs an algorithms to solve an Combinatorial Optimization Problem
 * 
 * <p> Therefore to design a new solver, simply have it implement ISolver.
 * 
 */

public interface ISolver {
	 property sz():Long;
	 
	 /**
	 * Solves the problem, which is specified by cop.
	 */
	 public def solve( cop : ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean) : Int;
	 
	 /**
	  *  Set the seed used fotr the random number generator.
	  */
	 public def setSeed(seed:Long):void;
	 
	 /**
	  * 	Clean solver variables to prepare a new solver execution.
	  */
	 public def clear():void;
	 
	 /**
	  * 	Return the Configuration with the best cost
	  */
	 public def getBestConfiguration():Valuation(sz);
	 
	 /**
	  * 	Return the cost of the best Configuration
	  */
	 public def getBestCost():Int;
	 
	 /**
	  * 	Stop the current solving process
	  */
	 public def kill():void;
	 
	 /**
	  * 	Report statistics from the solving process
	  */
	 public def reportStats(c:CSPStats):void;
	 
	 public def forceRestart():void;
	 
	 public def forceReset():void;
	 
}
public type ISolver(s:Long)=ISolver{self.sz==s};