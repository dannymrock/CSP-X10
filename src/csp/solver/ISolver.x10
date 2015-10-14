package csp.solver;
import csp.model.ModelAS;
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
	 def solve( cop : ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean) : Long;
	 
	 /**
	  *  Initialize all variables of the Solver
	  */
	 def initVar( cop_:ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean):void;
	
	 /**
	  *  Search process (in loop functionality) 
	  *  Implements an iteration of search process
	  */
	 def search( cop_ : ModelAS{self.sz==this.sz}) : Long;
	 
	 /**
	  *  Set the seed used for the random number generator.
	  */
	 def setSeed(seed:Long):void;
	  
	 /**
	  *  Interact with other entities
	  */
	 def interact( cop_:ModelAS{self.sz==this.sz}):void;
	 
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
	 def restartVar(cop : ModelAS):void;
	   
	 def updateCosts(cop : ModelAS):void;
	  
	 def updateTotStats():void;

	 def forceRestart():void;
	 
	 def forceReset():void;
}
public type ISolver(s:Long)=ISolver{self.sz==s};