/* 
 * COP-X10: An X10 Implementation of the CPMH framework
 * 
 * MIT License
 *
 * Copyright (c) 2022 Danny Munera
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package csp.solver;
import csp.model.GenericModel;
import x10.util.Random;
import csp.util.Logger;
import csp.model.ParamManager;
import x10.io.File;
import csp.util.Utils;
import csp.model.Main;
import x10.util.StringUtil;

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
	 
	 protected var updateI:Int;
	 protected var reportI:Int;// = (sz* Math.log(sz)) as Int ;//10n; 
	 protected var adaptiveComm:Boolean = false;
	 protected val modParams:Int;
	 
	 protected var costLR:Long = Long.MAX_VALUE;
	 protected var mySolverType:Int = Main.RS_SOL;
	 protected var maxUpdateI : Int = 100000n;
	 protected val changeOnDiver : Int;
	 
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
		  
		  this.modParams = this.opts("-M", 1n);
		  this.changeOnDiver = this.opts("-CD", 1n);
		  
		  val rep = this.opts( "-R", 0n );
		  val upd = this.opts( "-U", 0n );
		  
		  this.adaptiveComm = ( rep == -1n );
		  
		  reportI =  adaptiveComm ? ((sz* Math.log(sz)) as Int) : rep;
		  //reportI =  adaptiveComm ? sz as Int : rep;
		  updateI =  adaptiveComm ? (2n * reportI) : upd;
		  
		  val mustr = System.getenv("MU");
		  if (mustr != null)
				maxUpdateI = StringUtil.parseInt(mustr);
	 }

	 /**
	  * Solves the problem, which is specified by cop.
	  */
	 public def solve( cop : GenericModel{self.sz==this.sz}, tCost : Long, sLow: Boolean) : Long {
		  // Initialize all variables of the search process
		  initVar(cop, tCost, sLow);
		  // Initialize Cost
		  this.currentCost = cop.costOfSolution(true);
		  // Copy the first match to bestConf vector
		  Rail.copy(cop.getVariables(), bestConf as Valuation(sz));
		  if (this.currentCost == 0)
				bestCost = currentCost;
		  else
				bestCost = x10.lang.Int.MAX_VALUE;
		  
		  // Main Loop 
		  while( this.currentCost != 0 ){
				if (this.nIter >= this.maxIters){
					 //restart or finish
					 if(nRestart >= maxRestarts){
						  break;
					 }else{
						  nRestart++;
						  cop.initialize(); 
						  currentCost = cop.costOfSolution(true);
						  updateTotStats();
						  restartVar();
						  continue;
					 }
				}
				nIter++;
				this.currentCost = search( cop );
				
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
				
				/**
				 *  Report 
				 */
				// if(nIter % 10n == 0n)
				// 	 Console.OUT.println("iter\t"+nIter+"\tCurrent_Cost\t"+ currentCost + 
				// 				"\tBest_Cost\t" + bestCost);
		  }
		  
		  updateTotStats();
		  
		  return bestCost;
	 }
	 
	 /**
	  *  Initialize variables of the solver
	  *  Executed once before the main solving loop
	  */
	 protected def initVar( cop_:GenericModel{self.sz==this.sz}, tCost : Long, sLow: Boolean){
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
		  
		  this.costLR = Long.MAX_VALUE;;
		  
		  if (this.adaptiveComm)
				this.updateI = 2n * this.reportI;
		  
	 }
	 
	 /**
	  *  Search process (in loop functionality)
	  *  To be overwrited for each child class (solver) 
	  */
	 protected def search( cop_ : GenericModel{self.sz==this.sz}) : Long{
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
	  *  Set the seed used for the random number generator.
	  */
	 public def setSeed(seed:Long){
		  this.seed = seed;
		  random = new Random(seed);
	 }
	 
	 
	 /**
	  *  Interact with other entities
	  */
	 protected def interact( cop_:GenericModel{self.sz==this.sz}){
		  
		  // Interaction with other places
		  if( this.reportI != 0n && this.nIter % this.reportI == 0n){
				if(!bestSent){ 
					 val solverState = createSolverState();
					 solver.communicate(new State(sz,this.bestCost, this.bestConf as Valuation(sz), here.id as Int, solverState ));
					 bestSent = true;
				}
				else{
					 if (random.nextInt(reportI) == 0n){
						  val solverState = createSolverState();
						  solver.communicate(new State(sz,this.currentCost, cop_.getVariables() as Valuation(sz), here.id as Int, solverState));
					 }		  
				}
		  }
		  
		  if( this.updateI != 0n && this.nIter % this.updateI == 0n ){
				if ( this.adaptiveComm && this.updateI < maxUpdateI ){ 
					 this.updateI *= 2n;
					 // Console.OUT.println(here+" updateI " + updateI);
				}
				//Console.OUT.println("update");
				val result = solver.getIPVector(cop_, this.currentCost );
				if (result) {
					 this.nChangeV++;
					 this.currentCost = cop_.costOfSolution(true);
					 restartVar();
					 //Console.OUT.println("Changing vector in "+ here);
				} 
		  }
		  
		  // Force Restart: Inter Team Communication
		  if (this.forceRestart){
				//restart
				Logger.info(()=>{"   AdaptiveSearch : force Restart"});
				this.forceRestart = false;
				this.nForceRestart++;
				// get a new conf according the diversification approach
				val result = this.solver.getPR();
				if (result != null){	
					 
					 if(this.changeOnDiver == 1n) {
						  cop_.setVariables(result().vector);
						  this.currentCost = cop_.costOfSolution(true);
						  restartVar();
					 }
					 
					 if(this.modParams == 1n)
						  processSolverState(result().solverState);
				} else {
					 if(this.changeOnDiver == 1n) {
						  cop_.initialize();
						  this.currentCost = cop_.costOfSolution(true);
						  restartVar();
					 }
				}
				
				
				// restart self-adaptive UR params
				// if ( this.adaptiveComm ){
				// 	 this.reportI = (sz* Math.log(sz)) as Int;
				// 	 this.updateI = 2n * reportI;//sz as Int * 2n;
				// }
		  }
	 }
	
	 /**
	  *  Create Solver State array to be send to Pool
	  */
	 protected def createSolverState( ) : Rail[Int]{self.size==3}{
		  val rsState = new Rail[Int](3,-1n);
		  rsState(0) = this.mySolverType;
		  return rsState;  
	 }
	 
	 /**
	  *  Process Solver State Array received from Pool
	  * 
	  */
	 protected def processSolverState( state : Rail[Int]{self.size==3}){
		  // Random Search has no parameters to process
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
	 public def reportStats( c : GlobalStats){
		  c.iters = this.nIterTot;
		  c.swaps = this.nSwapTot;
		  c.vectorSize = this.sz;
		  c.target = this.targetSucc;
		  c.cost = this.bestCost;
		  c.restart = this.nRestart;
		  c.change = this.nChangeV;
		  c.forceRestart = this.nForceRestart;
		  val state = createSolverState();
		  c.sstate = state;
	 }
	 
	 /**
	  *  Restart solver state variables
	  */
	 protected def restartVar(){
		  bestSent = false;
	 }
	 
	 protected def updateCosts(cop : GenericModel){
		  if(this.currentCost < this.bestCost){ //(totalCost <= bestCost)
				Rail.copy(cop.getVariables() as Valuation(sz), this.bestConf as Valuation(sz));
				this.bestCost = this.currentCost;
				
				bestSent = false; // new best found, I must send it!
				
				if (this.reportPart){
					 val eT = (System.nanoTime() - initialTime)/1e9;
					 val gap = (this.bestCost-this.target)/(this.bestCost as Double)*100.0;

					 Utils.show("Solution",this.bestConf);
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
		  nSwap = 0n;
		  nIter = 0n;
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
