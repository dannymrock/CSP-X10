package csp.solver;

import csp.util.Logger;
import x10.util.Random;
import csp.model.ModelAS;
import x10.util.StringUtil; 
import x10.util.RailUtils;


/** EOSolver is the implementation of the Extremal Optimization solver
 * 	in the x10 lenguage.
 *  This x10 code is an adaptation of the 
 * 
 *  @author Danny Munera
 *  @version 0.1 - June 2015 -> first version
 * 	
 */

public class EOSolver(sz:Long, size:Int, solver:IParallelSolver(sz), mTime:Long)
implements ISolver
{
	 property sz()=sz;
	 private var target : Long = 0;
	 private var strictLow : Boolean = false;
	 private var targetSucc : Boolean = false;
	 
	 private var random : Random;
	 private var forceRestart : Boolean = false;
	 private var forceReset : Boolean = false;
	 
	 /**	Statistics	*/
	 private var nbRestart : Int = 0n;
	 private var nbForceRestart : Int = 0n;
	 private var nbIter : Int;
	 private var nbReset : Int;
	 private var nbSwap : Int;
	 private var nbSameVar : Int;
	 private var nbLocalMin : Int;
	 
	 /** Number time to change vector due to communication */ 
	 private var nbChangeV : Int = 0n;
	 
	 /** Total Statistics */
	 private var nbIterTot : Int;
	 private var nbResetTot : Int;	
	 private var nbSwapTot : Int;
	 private var nbSameVarTot : Int;
	 private var nbLocalMinTot : Int; 
	 private var nbInPlateau:Int; 
	 
	 /**
	  * Optimization mode 
	  */
	 private val bestConf = new Rail[Int](size, 0n);
	 private var bestCost:Int = x10.lang.Int.MAX_VALUE;
	 //private var bestOfBest:Int;
	 private var bestSent:Boolean=false;
	 
	 private var kill:Boolean = false;
	 private var seed:Long;	
	 private val maxTime = mTime;
	 
	 // PDF for EO
	 private val pdf = new Rail[Int](size, 0n);
	 private val fit = new Rail[PairAS] (size); 
	 //Here i -> index   j->cost (fitness)
	 private val cmp : (PairAS,PairAS) => Int = (a:PairAS,b:PairAS) => {return b.j - a.j ;}; 
	 
	 public def setSeed(seed:Long){
		  this.seed = seed;
		  random = new Random(seed);
	 }
	 
	 /**
	  *  solve( cop : ModelAS ) : Int
	  *  Solve a cop Problem through the Extremal Optimization algorithm
	  *  @param cop The model of the problem to solve
	  *  @return the final best cost after solving process
	  */ 
	 public def solve( cop_ : ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean) : Int { 
		  
		  val tStr = System.getenv("T");
		  val tau = (tStr==null)? (1.0 + 1.0 / Math.log(size)) : StringUtil.parseLong(tStr)/100.0;
		  // EO
		  //val tau = 1.0 + 1.0 / Math.log(size);
		  //val tau = 1.5;
		  //Console.OUT.println("tau "+tau);
		  inittau( tau, size );
		  Logger.debug(()=>{"EOSolver"});

		  
		  // Set Target
		  target = tCost;
		  this.strictLow = sLow;
		  targetSucc = false;
	
		  cop_.initialize(0n); //base value
		  //Main.show("initial= ",csp.variables);
		  nbRestart = 0n;
		  nbSwap = 0n;
		  nbIter = 0n;
		  nbSameVar = 0n;
		  nbLocalMin = 0n;
		  nbReset = 0n;
		  nbChangeV = 0n;
		  nbInPlateau = 0n;
		  nbIterTot = 0n;
		  nbResetTot = 0n;	
		  nbSwapTot = 0n;
		  nbSameVarTot = 0n;
		  nbLocalMinTot = 0n; 
		  nbForceRestart = 0n;
		  
		  var totalCost:Int = cop_.costOfSolution(true);
		  var newCost:Int = -1n;
		  var bestOfBest:Int = x10.lang.Int.MAX_VALUE;
		  
		  // Copy the first match to bestConf vector
		  Rail.copy(cop_.getVariables(), bestConf as Valuation(sz));
		  if (totalCost == 0n)
				bestCost = totalCost;
		  else
				bestCost = x10.lang.Int.MAX_VALUE;
		  //Console.OUT.println("initial bestCost="+bestCost);
		  
		  bestSent = false;
		  var initialTime:Long = System.nanoTime();
		  
		  // Selection variables
		  val eoi = new EOInfo();
		  
		  while( totalCost != 0n ){
				nbIter++;
				
				selectFirstVar( cop_, eoi );
				//Console.OUT.print("maxI= "+maxI);
				newCost = selectVarMinConflict( cop_, totalCost, eoi);
				//newCost = selectSecondVar( cop_ , totalCost, eoi);
				
				cop_.swapVariables(eoi.getFirstV(), eoi.getSecondV()); //adSwap(maxI, minJ,csp);
				nbSwap++;
				cop_.executedSwap(eoi.getFirstV(), eoi.getSecondV());
				totalCost = newCost; 
				//Console.OUT.println("swap "+maxI+" <-> "+minJ);
				 
				/**
				 * --- Interaction with other solvers -----
				 */
				
				Runtime.probe();		// Give a chance to the other activities
				if (kill){	//if (kill.get()){ 
					 //Logger.debug(()=>" killed!");
					 break;		// Check if other place or activity have finished
				}
				
				/**
				 *  optimization
				 */
				if(totalCost < bestCost){ //(totalCost <= bestCost)
					 Rail.copy(cop_.getVariables(), bestConf as Valuation(sz));
					 bestCost = totalCost;
					 bestSent = false;
					 // Console.OUT.println(here+" best cost= "+bestCost);
					 // Compare cost and break if target is accomplished
					 if ((strictLow && bestCost < target)||(!strictLow && bestCost <= target)){
						  targetSucc = true;
						  break;
					 }
				}
				
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
				
				/**
				 *  Interaction with other places
				 */
				if( solver.intraTISend() != 0n && nbIter % solver.intraTISend() == 0n){        //here.id as Int ){
					 if(!bestSent){ 
						  solver.communicate( bestCost, bestConf as Valuation(sz));
						  bestSent = true;
					 }else{
						  solver.communicate( totalCost, cop_.getVariables());
					 }
				}
				if(solver.intraTIRecv() != 0n && nbIter % solver.intraTIRecv() == 0n){        //here.id as Int ){
					 val result = solver.getIPVector(cop_, totalCost );
					 if (result){
						  nbChangeV++;
						  totalCost = cop_.costOfSolution(true);
						  bestSent = true;
						  //Console.OUT.println("Changing vector in "+ here);
					 }
				}
				
				
				
				/**
				 *  Force Restart: Inter Team Communication
				 */
				if (forceRestart){
					 //restart
					 Logger.info(()=>{"   ASSolverPermut : force Restart"});
					 forceRestart = false;
					 nbForceRestart++;
					 //restartVar(cop_);
					 continue;
				}
				
				if (forceReset){
					 //reset
					 Logger.info(()=>{"   ASSolverPermut : force Reset"});
					 forceReset = false;
					 nbForceRestart++;
					 //doReset(size as Int / 8n , cop_);
					 //doReset(nbVarReset as Int , cop_); // This reset should be bigger than the normal reset
					 continue;
				}	
				// ----- end of interaction with other solvers -----
		  } // while (totalCost != 0n)
		  
		  nbIterTot += nbIter;
		  nbResetTot += nbReset;	
		  nbSwapTot += nbSwap;
		  nbSameVarTot += nbSameVar;
		  nbLocalMinTot += nbLocalMin; 
		  
		  //cop_.displaySolution();
		  //Loger.info(()=>{"   ASSolverPermut: Finish search with best cost: "+bestCost+" kill="+kill });
		  
		  // if (bestCost == 0n){
		  // 	//Loger.info(()=>{"perfect solution found "});
		  // 	cop_.displaySolution(bestConf as Valuation(sz));
		  // }
		  // else{
		  // 	Logger.debug(()=>{"Best marriage found - BP= "+bestnbBP+" Singles="+bestnbSG});
		  // 	//cop_.displaySolution(bestConf as Valuation(sz));
		  // }
		  
		  
		  //creating Artificial errors for testing purposes
		  //cop_.swapVariables(1n,150n);
		  //cop_.swapVariables(1n,2n);
		  //bestConf(2)=2n;
		  //val tmp = bestConf(2);
		  //bestConf(2)=bestConf(3);
		  //bestConf(3)=tmp;
		  
		  
		  //Rail.copy(cop_.getVariables(),bestConf as Valuation(sz));
		  
		  return bestCost;
	 }
	 
	 

	 private def inittau(tau:Double, nv:Double){
		  var i:Int;
		  var a:Double,b:Double;
		  
		  a = (1 - Math.pow( nv + 1, 1 - tau ))/size as Double;
		  b = 1 / (1 - tau);
		  for (var k:Int=0n; k < size; k++){
				pdf(k) = (Math.pow(1 - k * a, b)) as Int;
				//Console.OUT.println("pdf="+k+" = "+pdf(k));
		  }
	 }
	 
	 private def taupick():Int{
		  return pdf(random.nextInt(size)) - 1n;
	 }
	 
	 private def selectFirstVar( cop_ : ModelAS, eoi: EOInfo){
		  var i: Int =-1n;
		  var cost: Int;
		  var selIndex:Int=0n; 
		  
		  while((i = cop_.nextI(i)) as UInt < size as UInt) { //False if i < 0
				cost = cop_.costOnVariable(i);
				fit(i) = new PairAS(i as Int , cost);
		  }
		  //[PairAS]
		  RailUtils.sort(fit, cmp);	
		  
		  
		  val index = taupick();
 
		  // selIndex = fit(index).i;
		  // eoi.setFirstV(selIndex);

		  val selFit = fit(index).j;
		  var nSameFit:Int = 0n;
		  for(var k:Int=0n; k < size; k++){
				if (fit(k).j < selFit)   // descending order
					 break;
				
				if (fit(k).j == selFit && random.nextInt(nSameFit)==0n)
					 selIndex = fit(k).i;
		  }
		  
		  eoi.setFirstV(selIndex);
		  		  
	 } 
	 
	 /**
	  * 	selectVarMinConflict( csp : ModelAS) : Int
	  * 	Computes swap and selects the minimum of cost if swap
	  * 	@param csp problem model
	  * 	@return the index of the variable with minimum individual cost if swap
	  */
	 private def selectVarMinConflict( csp : ModelAS, totalCost:Int, eoi:EOInfo) : Int {
		  var j: Int;
		  var cost: Int;
		  var minJ : Int = 0n;
		  var nSameMin:Int = 0n;
		  var minCost:Int = Int.MAX_VALUE;
		  
		  val fv = eoi.getFirstV();
		  
		  //Console.OUT.println("fv = "+ fv+" totalcost "+ totalCost);
		  
		  for (j = 0n; j < size; j++)
		  {	
				if (fv == j) continue;
				
				cost = csp.costIfSwap(totalCost, j, fv);
				//Console.OUT.println("J = "+ j+" cost "+ cost);
					
				if (cost < minCost){
					 minCost = cost;
					 minJ = j;
					 nSameMin = 1n;
				} else if (cost == minCost && random.nextInt(++nSameMin) == 0n){
					 minJ = j;
				}
		  }
		  //Console.OUT.println("minJ = "+ minJ+" newCost "+ minCost+" totalcost "+ totalCost);
		  eoi.setSecondV(minJ);
		  return minCost;
	 }
	 
	 private def selectSecondVar( csp : ModelAS, totalCost:Int, eoi:EOInfo) : Int {
		  val randomJ = random.nextInt(size);
		  val newCost = csp.costIfSwap(totalCost, randomJ, eoi.getFirstV());	 
		  eoi.setSecondV(randomJ);
		  return newCost; 
	 }
	 
	 /**
	  * 	Clear function
	  */
	 public def clear(){
		  this.kill = false;
	 }
	 
	 public def forceRestart(){
		  Logger.info(()=>"EOSolver: Force Restart True");
		  forceRestart = true;
	 }
	 public def forceReset(){
		  Logger.info(()=>"EOSolver: Force Reset True");
		  forceReset = true;
	 }
	 
	 // public def restartVar(csp : ModelAS){
		//   //Logger.info(()=>"ASSolver Permut: Restart");
		//   csp.initialize(0n); // Random Permut
		//   totalCost = csp.costOfSolution(true);
		//   bestOfBest = x10.lang.Int.MAX_VALUE ;
		//   Rail.copy(csp.getVariables() as Valuation(sz),bestConf as Valuation(sz));
		//   bestCost = totalCost;
		//   bestSent = false;
		//   nbInPlateau = 0n;
		//   
		//   //Not sure if this is necessary
		//   solver.clearPool();//??? Restart only the pool		
		//   
		//   //nbRestart++;			
		//   //Update Total statistics
		//   nbIterTot += nbIter;
		//   nbResetTot += nbReset;        
		//   nbSwapTot += nbSwap;
		//   nbSameVarTot += nbSameVar;
		//   nbLocalMinTot += nbLocalMin; 
		//   //Restart local var
		//   nbSwap = 0n;
		//   nbIter = 0n;
		//   nbSameVar = 0n;
		//   nbLocalMin = 0n;
		//   nbReset = 0n;
	 // }
	 
	 /**
	  * 	Return the array of variables with the best cost
	  */
	 public def getBestConfiguration():Valuation(sz){
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
		  c.locmin = nbLocalMinTot;
		  c.swaps = nbSwapTot;
		  c.reset = nbResetTot;
		  c.same = nbSameVarTot;
		  c.restart = nbRestart;
		  c.change = nbChangeV;
		  val singles = bestCost % size;
		  c.singles = singles;
		  c.bp = (bestCost-singles)/size;
		  c.forceRestart = nbForceRestart;
		  c.target = targetSucc;
		  c.cost = bestCost;
	 }
	 
}
public type EOSolver(s:Long)=EOSolver{self.sz==s};