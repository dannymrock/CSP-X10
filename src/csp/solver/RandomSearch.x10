package csp.solver;
import csp.model.ModelAS;
import x10.util.Random;
import csp.util.Logger;
import csp.model.ParamManager;

/**
 * Basic Implementation of a Random Search Solver
 * 
 * <p> Therefore to design a new iterative search solver, simply have it extend RandomSearch
 * 
 */

public class RandomSearch(sz:Long){
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
	 protected var nSwap : Int;
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
		  
	 }

	 /**
	  * Solves the problem, which is specified by cop.
	  */
	 public def solve( cop : ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean) : Long {
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
	  *  Initialize variables of the solver
	  */
	 protected def initVar( cop_:ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  // Set Target
		  this.target = tCost;
		  this.strictLow = sLow;
		  this.targetSucc = false;
		  cop_.initialize(); 
		  //Main.show("initial= ",csp.variables);
		  this.nSwap = 0n;
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
	 protected def search( cop_ : ModelAS{self.sz==this.sz}) : Long{
		  // Swap two random variables
		  //Console.OUT.println("HS");
		  move.setFirst(random.nextLong(sz));
		  move.setSecond(random.nextLong(sz));
		  
		  cop_.swapVariables(move.getFirst(), move.getSecond());
		  nSwap++;
		  
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
				val result = solver.getIPVector(cop_, this.currentCost );
				if (result){
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
				}
		  }
		  
		  // if (this.forceReset){
		  // //reset
		  // Logger.info(()=>{"   ASSolverPermut : force Reset"});
		  // this.forceReset = false;
		  // this.nForceRestart++;
		  // //doReset(size as Int / 8n , csp_);
		  // this.doReset(this.nVarToReset , cop_); // This reset should be bigger than the normal reset
		  // }
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
	 public def getBestConfiguration():Valuation(sz){
		  return this.bestConf as Valuation(sz);
	 }
	 
	 /**
	  * 	Return the cost of the best Configuration
	  */
	 public def getBestCost():Long{
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
		  c.iters = this.nIterTot;
		  c.swaps = this.nSwapTot;
		  c.vectorSize = this.sz;
		  c.target = this.targetSucc;
		  c.cost = this.bestCost;
		  c.restart = this.nRestart;
		  c.change = this.nChangeV;
		  c.forceRestart = this.nForceRestart;
	 }
	 
	 protected def restartVar(cop : ModelAS){
		  cop.initialize(); 
		  currentCost = cop.costOfSolution(true);
		  updateTotStats();
		  bestSent = false;
		  nSwap = 0n;
		  nIter = 0n;
	 }
	 
	 // protected def updateCosts(cop : ModelAS){
		//   /**
		//    *  optimization
		//    */
		//   if(this.currentCost < this.bestCost){ //(totalCost <= bestCost)
		// 		Rail.copy(cop.getVariables(), this.bestConf as Valuation(sz));
		// 		this.bestCost = this.currentCost;
		// 		
		// 		if (this.reportPart){
		// 			 val eT = (System.nanoTime() - initialTime)/1e9;
		// 			 val gap = (this.bestCost-this.target)/(this.bestCost as Double)*100.0;
		// 			 Console.OUT.printf("%s\ttime: %5.1f s\tbest cost: %10d\tgap: %5.2f%% \n",here,eT,this.bestCost,gap);
		// 		}
		// 		
		// 		// Compare cost and break if target is accomplished
		// 		if ((this.strictLow && this.bestCost < this.target)
		// 				  ||(!this.strictLow && this.bestCost <= this.target)){
		// 			 this.targetSucc = true;
		// 			 this.kill = true;
		// 		}
		//   }
	 // }
	 
	 protected def updateCosts(cop : ModelAS){
		  if(this.currentCost < this.bestCost){ //(totalCost <= bestCost)
				Rail.copy(cop.getVariables() as Valuation(sz), this.bestConf as Valuation(sz));
				this.bestCost = this.currentCost;
				
				bestSent = false; // new best found, I must send it!
				
				if (this.reportPart){
					 val eT = (System.nanoTime() - initialTime)/1e9;
					 val gap = (this.bestCost-this.target)/(this.bestCost as Double)*100.0;
					 Console.OUT.printf("%s\ttime: %5.1f s\tbest cost: %10d\tgap: %5.2f%% \n",here,eT,this.bestCost,gap);
				}
				
				// Console.OUT.println(here+" best cost= "+bestCost);
				// Compare cost and break if target is accomplished
				if ((this.strictLow && this.bestCost < this.target)
						  ||(!this.strictLow && this.bestCost <= this.target)){
					 this.targetSucc = true;
					 this.kill = true;
				}
		  }
	 }
	 
	 
	 
	 protected def updateTotStats(){
		  this.nIterTot += this.nIter;
		  this.nSwapTot += this.nSwap;  
	 }
	 
	 
	 public def forceRestart(){
		  Logger.info(()=>"ASSolver: Force Restart True");
		  forceRestart = true;
	 }
	 public def forceReset(){
		  Logger.info(()=>"ASSolver: Force Reset True");
		  forceReset = true;
	 }
	 
}
public type RandomSearch(s:Long)=RandomSearch{self.sz==s};