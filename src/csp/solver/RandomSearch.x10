package csp.solver;
import csp.model.ICOPModel;
import x10.util.Random;
import csp.util.Logger;
import csp.model.ParamManager;
import x10.io.File;

/**
 * Basic Implementation of a Random Search Solver
 * 
 * <p> Therefore to design a new iterative search solver, simply have it extend RandomSearch
 * 
 */

public class RandomSearch(sz:Long) implements ISolver {
	 property sz() = sz; //size of the problem
	 
	 //Parameters object
	 protected val opts: ParamManager;
	 
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
	 protected var nRestart : Int = 0n;
	 protected var nIter : Int;
	 //protected var nSwap : Int;
	 protected var nMoves : Int;
	 protected var nForceRestart : Int = 0n;
	 
	 /** Total Statistics */
	 protected var nIterTot : Int;
	 protected var nSwapTot : Int;
	 // -> Result
	 protected val bestConf:Rail[Int];
	 protected var bestCost:Long = Long.MAX_VALUE;
	 protected var currentCost:Long;
	 // -> Stop search process 
	 protected var kill:Boolean = false;
	 // -> Max time
	 protected val maxTime:Long;
	 protected var initialTime:Long;
	 // RESTART
	 protected var restart:Boolean = false;
	 protected var maxIters:Long;
	 protected var maxRestarts:Int;
	 
	 // not sure
	 protected var forceRestart : Boolean = false;
	 protected var forceReset : Boolean = false;
	 
	 // Report results
	 protected val reportPart:Boolean;
	 
	 
	 protected val solver:IParallelSolver(sz); 
	 /** Number time to change vector due to communication */ 
	 protected var nChangeV : Int = 0n;
	 protected var bestSent:Boolean=false;
	 
	 protected val altTty:File;
	 
	 protected val maxDomain = 10; 
	 
	 
	 public def this(size:Long, solver:IParallelSolver(size), opt:ParamManager){
		  property(size);
		  //this.vectorSize = size;
		  this.opts = opt;
		  this.bestConf = new Rail[Int](this.sz, 0n);
		  this.solver = solver;
		  // Parameters
		  this.maxTime = this.opts("-mt", 0);
		  this.maxIters = this.opts("-mi", 100000000);
		  this.maxRestarts = this.opts("-mr", 0n);
		  this.reportPart = this.opts("-rp", 0n) == 1n;
		  this.altTty = new File("/dev/pts/1");
		  
	 }

	 /**
	  * Solves the problem, which is specified by cop.
	  * From ISolver Interface
	  */
	 public def solve( cop : ICOPModel{self.sz==this.sz}, tCost : Long, sLow: Boolean) : Long {
		  // Initialize all variables of the search process
		  initVar(cop, tCost, sLow);
		  // Initialize Cost
		  this.currentCost = cop.costOfSolution(true);
		  // Copy the first match to bestConf vector
		  Rail.copy(cop.getVariables(), bestConf as Valuation(sz));
		  if (currentCost == 0)
				bestCost = currentCost;
		  else
				bestCost = x10.lang.Int.MAX_VALUE;
		  
		  // Main Loop 
		  while( currentCost != 0 ){
				if (this.nIter >= this.maxIters){
					 //restart or finish
					 if(nRestart >= maxRestarts){
						  break;
					 }else{
						  nRestart++;
						  restartVar( cop );
						  continue;
					 }
				}
				nIter++;
				currentCost = search( cop );
				
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
						  Logger.debug(()=>{" Time Out"});
						  //Console.OUT.println(" Time Out");
						  break;
					 }
				}
				
				/**
				 *  Possible interaction with other solvers
				 */
				interact(cop);
				
		  }
		  
		  updateTotStats();
		  
		  return bestCost;
	 }
	 
	 /**
	  *  Set the seed used fotr the random number generator.
	  *  From ISolver Interface
	  */
	 public def setSeed(seed:Long){
		  this.seed = seed;
		  random = new Random(seed);
	 }
	 
	 /**
	  * 	Clean solver variables to prepare a new solver execution.
	  *  From ISolvez interface
	  */
	 public def clear(){
		  this.kill = false;
	 }
	 
	 /**
	  *  Return the Configuration with the best cost
	  *  From ISolver Interface
	  */
	 public def getBestConfiguration():Valuation(sz){
		  return this.bestConf as Valuation(sz);
	 }
	 
	 /**
	  * Return the cost of the best Configuration
	  * From ISolver Interface
	  */
	 public def getBestCost():Long{
		  return this.bestCost;
	 }
	 
	 /**
	  *  Stop the current search process
	  *  From ISolver Interface
	  */
	 public def kill(){
		  this.kill = true;
	 }
	 
	 /**
	  *  Report statistics from the solving process
	  *  From ISolver Interface
	  */
	 public def reportStats( c : CSPStats){
		  c.iters = this.nIterTot;
		  c.swaps = this.nSwapTot;
		  c.vectorSize = this.sz;
		  c.target = this.targetSucc;
		  c.cost = this.bestCost;
		  c.restart = this.nRestart;
		  c.change = this.nChangeV;
		  c.forceRestart = this.nForceRestart;
	 }
	 
	 public def forceRestart(){
		  Logger.info(()=>"ASSolver: Force Restart True");
		  forceRestart = true;
	 }
	 public def forceReset(){
		  Logger.info(()=>"ASSolver: Force Reset True");
		  forceReset = true;
	 }
	 
	 /**
	  *  Initialize variables of the solver
	  */
	 protected def initVar( cop_:ICOPModel{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  // Set Target
		  this.target = tCost;
		  this.strictLow = sLow;
		  this.targetSucc = false;
		  cop_.initialize(); 
		  //Main.show("initial= ",csp.variables);
		  this.nMoves = 0n;
		  this.nIter = 0n;
		  this.nRestart = 0n;
		  
		  // clear Tot stats
		  this.nIterTot = 0n;
		  this.nSwapTot = 0n;
		  this.initialTime = System.nanoTime();
		  // Comm
		  this.bestSent = false;
		  this.nForceRestart = 0n;
		  this.nChangeV = 0n;
		  
	 }
	 
	 /**
	  *  Search process (in loop functionality)
	  *  To be overwrited for each child class (solver) 
	  */
	 protected def search( cop_ : ICOPModel{self.sz==this.sz}) : Long{
		  // Swap two random variables
		  //Console.OUT.println("HS");
		  //move.setFirst(random.nextLong(sz));
		  //move.setSecond(random.nextLong(sz));
		  val i = random.nextLong(sz); // worst variable
		  
		  //Change its value on its domain
		  val value = random.nextInt(cop_.getMaxDomain()+1n)+cop_.getMinDomain();
		  
		  cop_.executeMove(i, value);
		  //cop_.swapVariables(move.getFirst(), move.getSecond());
		  nMoves++;
		  
		  //cop_.executedSwap(move.getFirst(), move.getSecond());
		  return cop_.costOfSolution(true);
	 }	 
	 
	 /**
	  *  Interact with other entities
	  */
	 protected def interact( cop_:ICOPModel{self.sz==this.sz}){
		  // To be implemented  
		  // To be implemented  
		  /**
		   *  Interaction with other places
		   */
		  if( solver.inTeamUpdateI() != 0n && this.nIter % solver.inTeamUpdateI() == 0n){        //here.id as Int ){
				if(!bestSent){ 
					 solver.communicate( this.bestCost, this.bestConf as Valuation(sz));
					 bestSent = true;
				}else{
					 solver.communicate( this.currentCost, cop_.getVariables());
				}
		  }
		  
		  if(solver.inTeamReportI() != 0n && this.nIter % solver.inTeamReportI() == 0n){        //here.id as Int ){
				val c = new Rail[Int](sz, 0n);
				val result = solver.getIPVector(c);
				if (result){
					 cop_.setVariables(c);
					 //this.currentCost = cop_.costOfSolution(true);
					 this.nChangeV++;
					 this.currentCost = cop_.costOfSolution(true);
					 bestSent = true;
					 //Console.OUT.println("Changing vector in "+ here);
				}
		  }
		  
		  /**
		   *  Force Restart: Inter Team Communication
		   */
		  if (this.forceRestart){
				//restart
				Logger.info(()=>{"   AdaptiveSearch : force Restart"});
				this.forceRestart = false;
				this.nForceRestart++;
				// PATH RELINKING-based approach
				val c = new Rail[Int](sz, 0n);
				
				val result = this.solver.getPR(c);
				
				if (result){
					 cop_.setVariables(c);
					 this.currentCost = cop_.costOfSolution(true);
					 this.bestSent = true;
				}else{
					 cop_.initialize();
					 this.currentCost = cop_.costOfSolution(true);
					 this.bestSent = true;
				}
		  }
	 }
	
	 protected def restartVar(cop : ICOPModel){
		  cop.initialize(); 
		  currentCost = cop.costOfSolution(true);
		  updateTotStats();
		  bestSent = false;
		  nMoves = 0n;
		  nIter = 0n;
	 }
	 
	 protected def updateCosts(cop : ICOPModel){
		  if(this.currentCost < this.bestCost){ //(totalCost <= bestCost)
				Rail.copy(cop.getVariables() as Valuation(sz), this.bestConf as Valuation(sz));
				this.bestCost = this.currentCost;
				bestSent = false; // new best found, I must send it!
				if ((this.strictLow && this.bestCost < this.target)
						  ||(!this.strictLow && this.bestCost <= this.target)){
					 this.targetSucc = true;
					 this.kill = true;
				}
		  }
	 }
	 
	 protected def updateTotStats(){
		  this.nIterTot += this.nIter;
		  this.nSwapTot += this.nMoves;  
	 }
	 
}
public type RandomSearch(s:Long)=RandomSearch{self.sz==s};