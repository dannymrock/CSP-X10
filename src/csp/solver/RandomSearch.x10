package csp.solver;
import csp.model.ModelAS;
import x10.util.Random;
import csp.util.Logger;

/**
 * Basic Implementation of a Random Search Solver
 * 
 * <p> Therefore to design a new iterative search solver, simply have it extend RandomSearch
 * 
 */

public class RandomSearch(sz:Long, size:Int, mTime:Long){
	 property sz() = sz;
	 
	 // Move information
	 protected val move = new MovePermutation(-1n, -1n);
	 
	 // Random Number Generator
	 protected var random : Random;
	 protected var seed:Long;	
	 
	 // Variables
	 // -> Target
	 protected var target : Long = 0;
	 protected var strictLow : Boolean = false;
	 protected var targetSucc : Boolean = false;
	 
	 // -> Statistics
	 protected var nbRestart : Int = 0n;
	 protected var nbIter : Int;
	 protected var nbSwap : Int;
	 /** Total Statistics */
	 protected var nbIterTot : Int;
	 protected var nbSwapTot : Int;
	 // -> Result
	 protected val bestConf = new Rail[Int](size, 0n);
	 protected var bestCost:Int = x10.lang.Int.MAX_VALUE;
	 protected var currentCost:Int;
	 // -> Stop search process 
	 protected var kill:Boolean = false;
	 // -> Max time
	 protected val maxTime = mTime;
	 protected var initialTime:Long;
	 // RESTART
	 protected var restart:Boolean = false;
	 
	 // not sure
	 private var forceRestart : Boolean = false;
	 private var forceReset : Boolean = false;
	 

	 
	 /**
	  * Solves the problem, which is specified by cop.
	  */
	 public def solve( cop : ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean) : Int {
		  // Initialize all variables of the search process
		  initVar(cop, tCost, sLow);
		  
		  // Initialize Cost
		  this.currentCost = cop.costOfSolution(true);
		  // Copy the first match to bestConf vector
		  Rail.copy(cop.getVariables(), bestConf as Valuation(sz));
		  if (currentCost == 0n)
				bestCost = currentCost;
		  else
				bestCost = x10.lang.Int.MAX_VALUE;
		  
		  // Main Loop 
		  while( currentCost != 0n ){
				nbIter++;
				
				currentCost = search( cop );
				
				/**
				 *  Restart
				 */
				if (restart){
					 restart = false;
					 nbRestart++;
					 restartVar( cop );
					 continue;
				}
				
				/**
				 *  Update the best configuration found so far
				 */
				updateCosts(cop);
				
				/**
				 *  Kill solving process
				 */
				Runtime.probe();	// Give a chance to the other activities
				if (kill)
					 break;  // kill: End solving process
				
				/**
				 *  Time out
				 */
				if(maxTime > 0){
					 val eTime = System.nanoTime() - initialTime; 
					 if(eTime/1e6 >= maxTime){ //comparison in miliseconds
						  //Logger.info(()=>{" Time Out"});
						  break;
					 }
				}
		  }
		  nbIterTot += nbIter;
		  nbSwapTot += nbSwap;
		  
		  return bestCost;
	 }
	 
	 /**
	  *  Initialize variables of the solver
	  */
	 protected def initVar( cop_:ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  // Set Target
		  this.target = tCost;
		  this.strictLow = sLow;
		  this.targetSucc = false;
		  
		  cop_.initialize(0n); //base value
		  //Main.show("initial= ",csp.variables);
		  this.nbSwap = 0n;
		  this.nbIter = 0n;
		  this.nbIterTot = 0n;
		  this.nbSwapTot = 0n;
		  
		  this.initialTime = System.nanoTime();
		  
		  
	 }
	 
	 /**
	  *  Search process (in loop functionality)
	  *  To be overwrited for each child class (solver) 
	  */
	 protected def search( cop_ : ModelAS{self.sz==this.sz}) : Int{
		  
		  // Swap two random variables
		  //Console.OUT.println("HS");
		  
		  move.setFirst(random.nextInt(size));
		  move.setSecond(random.nextInt(size));
		  
		  cop_.swapVariables(move.getFirst(), move.getSecond());
		  nbSwap++;
		  
		  cop_.executedSwap(move.getFirst(), move.getSecond());
		  return cop_.costOfSolution(true);
	 }
	 
	 
	 
	 /**
	  *  Set the seed used fotr the random number generator.
	  */
	 public def setSeed(seed:Long){
		  this.seed = seed;
		  random = new Random(seed);
	 }
	 
	 
	 /**
	  *  Interact with other entities
	  */
	 protected def interact( cop_:ModelAS{self.sz==this.sz}){
		  // To be implemented  
	 }
	 
	 /**
	  * 	Clean solver variables to prepare a new solver execution.
	  */
	 public def clear(){
		  this.kill = false;
	 }
	 
	 /**
	  * 	Return the Configuration with the best cost
	  */
	 public def getBestConfiguration(){
		  return this.bestConf;
	 }
	 
	 /**
	  * 	Return the cost of the best Configuration
	  */
	 public def getBestCost():Int{
		  return this.bestCost;
	 }
	 
	 /**
	  * 	Stop the current search process
	  */
	 public def kill(){
		  this.kill = true;
	 }
	 
	 /**
	  * 	Report statistics from the solving process
	  */
	 public def reportStats( c : CSPStats){
		  c.iters = nbIterTot;
		  c.swaps = nbSwapTot;
		  val singles = bestCost % size;
		  c.singles = singles;
		  c.bp = (bestCost-singles)/size;
		  c.target = targetSucc;
		  c.cost = bestCost;
	 }
	 
	 protected def restartVar(cop : ModelAS){
		  cop.initialize(0n); //(solverP.baseValue); // Random Permut
		  currentCost = cop.costOfSolution(true);
		  nbIterTot += nbIter;
		  nbSwapTot += nbSwap;
		  nbSwap = 0n;
		  nbIter = 0n;
	 }
	 
	 protected def updateCosts(cop : ModelAS){
		  /**
		   *  optimization
		   */
		  if(this.currentCost < this.bestCost){ //(totalCost <= bestCost)
				Rail.copy(cop.getVariables(), this.bestConf as Valuation(sz));
				this.bestCost = this.currentCost;
				// Console.OUT.println(here+" best cost= "+bestCost);
				// Compare cost and break if target is accomplished
				if ((this.strictLow && this.bestCost < this.target)
						  ||(!this.strictLow && this.bestCost <= this.target)){
					 this.targetSucc = true;
					 this.kill = true;
				}
		  }
	 }
	 
	 
	 public def forceRestart(){
		  Logger.info(()=>"EOSolver: Force Restart True");
		  forceRestart = true;
	 }
	 public def forceReset(){
		  Logger.info(()=>"EOSolver: Force Reset True");
		  forceReset = true;
	 }
	 
}
public type RandomSearch(s:Long)=RandomSearch{self.sz==s};