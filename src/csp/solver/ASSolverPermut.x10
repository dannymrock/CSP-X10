package csp.solver;

import csp.util.Logger;
import csp.model.ASSolverParameters;
import x10.util.Random;
import x10.util.concurrent.AtomicBoolean;
import csp.model.ModelAS;
import x10.util.StringUtil; 
import x10.util.RailUtils;
import csp.util.Utils;


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

public class ASSolverPermut(sz:Long) implements ISolver
{
	 property sz()=sz;
	 private var target : Long = 0;
	 private var strictLow : Boolean = false;
	 private var targetSucc : Boolean = false;
	 
	 private val mark : Rail[Int]; 
	 // Solver parameters - Different values for every kind of problem
	 private val solverP = new ASSolverParameters();
	 
	 //var nb_var_to_reset : Int; 
	 
	 private var maxI : Int;		
	 private var minJ : Int;
	 
	 //var bestCost : Int;
	 private var newCost : Int;
	 private var totalCost : Int;
	 private var random : Random;
	 
	 
	 private var forceRestart : Boolean = false;
	 private var forceReset : Boolean = false;
	 
	 private var listInb : Int;
	 private var listJnb : Int;
	 
	 private val listIJ : Rail[PairAS];
	 private val listI : Rail[Int];
	 
	 private var nbVarMarked : Int = 0n; 
	 //val varRegion : Region(1);
	 /** Number of iterations to update kill status */
	 //val updateP : Int;
	 
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
	 
	 /** For Exhaustive search */
	 private var nbListIJ : Int;
	 
	 /**
	  * Optimization mode 
	  */
	 private val bestConf : Rail[Int];
	 private var bestCost:Int = x10.lang.Int.MAX_VALUE;
	 
	 private var bestOfBest:Int;
	 private var bestSent:Boolean=false;
	 
	 //val kill : AtomicBoolean;
	 //var kill:Boolean;
	 private var kill:Boolean = false;
	 private var seed:Long;	
	 // public def this(sz:Long, size:Int, seed:Long, solver:ParallelSolverI(sz)){
	 // 	property(sz, size, seed, solver);
	 // 	kill=new AtomicBoolean(false);
	 // 	//kill=false;
	 // }
	 
	 private val maxTime : Long;
	 
	 
	 // PDF for EO
	 private val pdf : Rail[Int];
	 private val fit : Rail[PairAS]; 
	 private val cmp : (PairAS,PairAS) => Int = (a:PairAS,b:PairAS) => {return b.j - a.j ;};
	 
	 
	 
	 private size:Int;
	 
	 private solver:IParallelSolver(sz); 
	 
	 private var pSendLM:Double = 0.5;
	 
	 public def this(sz:Long, size:Int, solver:IParallelSolver(sz), mTime:Long):ASSolverPermut(sz){
		  property(sz);
		  mark = new Rail[Int] (size, 0n);
		  listIJ = new Rail[PairAS](size);
		  listI = new Rail[Int](size, 0n);
		  bestConf = new Rail[Int](size, 0n);
		  maxTime = mTime;
		  pdf = new Rail[Int](size, 0n);
		  this.size = size;
		  this.solver=solver;
		  fit = new Rail[PairAS] (size);
		  
		  val str = System.getenv("LM");
		  if (str != null)
				pSendLM = StringUtil.parseInt(str)/ 100.0;
		  
	 }
	 
	 public def setSeed(seed:Long){
		  this.seed = seed;
		  random = new Random(seed);
	 }
	 
	 
	 /**
	  *  solve( csp : ModelAS ) : Int
	  *  Solve a csp Problem through the Adaptive Search algoritm
	  *  @param csp The model of the problem to solve
	  *  @return the final best cost after solving process
	  */ 
	 public def solve( csp_ : ModelAS{self.sz==this.sz()}, tCost : Long, sLow: Boolean) : Int { 
		  
		  //val tStr = System.getenv("T");
		  //val tau = (tStr==null)? (1.0 + 1.0 / Math.log(size)) : StringUtil.parseLong(tStr)/100.0;
		  // EO
		  //val tau = 1.0 + 1.0 / Math.log(size);
		  //val tau = 1.5;
		  
		  //Console.OUT.println("tau "+tau);
		  //inittau( tau, size );
		  Logger.debug(()=>{"ASSolver"});
		  
		  csp_.setParameters(solverP);
		  
		  target = tCost;
		  this.strictLow = sLow;
		  targetSucc = false;
		  
		  
		  //nb_var_to_reset = (((size * solverP.resetPercent) + (100) - 1) / (100));
		  if (solverP.nbVarToReset == -1n){
				solverP.nbVarToReset = (((size * solverP.resetPercent) + (100n) - 1n) / (100n));
				if (solverP.nbVarToReset < 2n){
					 solverP.nbVarToReset = 2n;
					 //Loger.debug(()=>{"increasing nb var to reset since too small, now = "+ solverP.nbVarToReset});
				}
		  }
		  
		  
		  val nStr = System.getenv("N");
		  val nbVarReset = (nStr==null)? 0 : StringUtil.parseLong(nStr);
		  
		  
		  csp_.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
		  //Main.show("initial= ",csp.variables);
		  
		  mark.clear();
		  listI.clear();
		  
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
		  
		  totalCost = csp_.costOfSolution(true);
		  bestOfBest = x10.lang.Int.MAX_VALUE;
		  
		  // Copy the first match to bestConf vector
		  Rail.copy(csp_.getVariables(),bestConf as Valuation(sz));
		  if (totalCost == 0n)
				bestCost = totalCost;
		  else
				bestCost = x10.lang.Int.MAX_VALUE;
		  //Console.OUT.println("initial bestCost="+bestCost);
		  
		  bestSent = false;
		  var initialTime:Long = System.nanoTime();
		  
		  while( totalCost != 0n ){
				nbIter++;
				
				if (nbIter >= solverP.restartLimit){
					 if(nbRestart < solverP.restartMax){
						  //restart
						  //Loger.debug(()=>"ASSolver:Restart");
						  forceRestart = false;
						  nbRestart++;
						  restartVar(csp_);
						  continue;
					 }
					 //Console.OUT.println("Not solution found");
					 break; 
				}
				
				if( !solverP.exhaustive ){
					 maxI = selectVarHighCost( csp_ );
					 //maxI = selectVarHighCostEO( csp_ );
					 //Console.OUT.print("maxI= "+maxI);
					 minJ = selectVarMinConflict( csp_ );
					 //minJ = select2VarEO( csp_ );
					 //Console.OUT.println("  minJ= "+minJ);
					 //Console.OUT.println("maxI "+maxI+"  minJ "+minJ+" cost "+totalCost+"  newCost "+newCost);
				} else {
					 selectVarsToSwap( csp_ );
					 //Console.OUT.println("maxI= "+maxI+"  minJ= "+minJ);
				}
				//Logger.debug(()=>{"----- iter no: "+nbIter+", cost: "+totalCost+", nb marked: "+nbVarMarked+" ---, nb_swap= "+nbSwap});
				//Console.OUT.println("----- iter no: "+nbIter+", cost: "+totalCost+", nb marked: "+nbVarMarked+" ---, nb_swap= "+nbSwap);
				
				if (totalCost != newCost) {
					 if (nbInPlateau > 1n) {
						  //Console.OUT.println("end of plateau, length: "+ nbInPlateau);
					 }
					 nbInPlateau = 0n;
				}
				//if (newCost < bestCost) bestCost = newCost;
				
				nbInPlateau++;
				
				if (minJ == -1n) continue;
				
				if (maxI == minJ) {
					 //val res = solverC.communicate(totalCost, csp,commRefs);
					 //if (minJ != alMinJ)
					 //Console.OUT.println("lminJ = "+ minJ+ " alMinJ = "+alMinJ);
					 
					 nbLocalMin++;
					 mark(maxI) = nbSwap + solverP.freezeLocMin; //Mark(maxI, freeze_loc_min);
					 //Console.OUT.println("nbVarMarked "+nbVarMarked+"solverP.resetLimit= "+solverP.resetLimit);
					 if (nbVarMarked + 1 >= solverP.resetLimit)
					 {				
						  // do reset or get some vector from the comm pool
						  //if (random.randomInt(100) < solverP.probChangeVector){
						  // val result = solverC.getIPVector(csp, totalCost );
						  // if (result == -1)
						  // doReset(solverP.nbVarToReset,csp);//doReset(nb_var_to_reset,csp);
						  // else{
						  // nbChangeV++;
						  // nbSwap += size ; //I don't know what happened here with costas reset
						  // mark.clear();
						  // totalCost = csp.costOfSolution(1);
						  // }
						  // }else{*/
						  //Console.OUT.println("\tTOO MANY FROZEN VARS - RESET");
						  //Utils.show("Local Min : ",csp_.getVariables());
						  //Console.OUT.println("cost = "+totalCost);
						  
						  // communicate Local Minimum
						  if (random.nextDouble() < pSendLM)
								solver.communicateLM( totalCost, csp_.getVariables());
						  
						  doReset(solverP.nbVarToReset,csp_);//doReset(nb_var_to_reset,csp);
						  //Main.show("after reset= ",csp.variables);
						  //}
						  
						  //Utils.show("after reset= ",csp_.getVariables());
					 }
				}else {
					 mark(maxI) = nbSwap + solverP.freezeSwap; //Mark(maxI, ad.freeze_swap);
					 mark(minJ) = nbSwap + solverP.freezeSwap; //Mark(minJ, ad.freeze_swap);
					 
					 csp_.swapVariables(maxI, minJ); //adSwap(maxI, minJ,csp);
					 nbSwap++;
					 csp_.executedSwap(maxI, minJ);
					 totalCost = newCost;
					 //Console.OUT.println("swap "+maxI+" <-> "+minJ);
				}
				
				// 	Utils.show("partial sol",csp_.getVariables());
				// csp_.displaySolution();			
				
				/**
				 * --- Interaction with other solvers -----
				 */
				
				Runtime.probe();		// Give a chance to the other activities
				if (kill){	//if (kill.get()){ 
					 //Logger.debug(()=>" killed!");
					 break;		// Check if other place or activity have finished
				}
				
				// print iter an cost
				//if (nbIter % 10n == 0n)
				// Console.OUT.println("i"+nbIter+"i\t"+(bestCost/100n)+"\t"+(bestCost%100n));
				// Console.OUT.println(nbIter+" "+(totalCost)+" "+(totalCost));
				
				/**
				 *  optimization
				 */
				if(totalCost < bestCost){ //(totalCost <= bestCost)
					 Rail.copy(csp_.getVariables(), bestConf as Valuation(sz));
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
						  solver.communicate( totalCost, csp_.getVariables());
					 }
				}
				if(solver.intraTIRecv() != 0n && nbIter % solver.intraTIRecv() == 0n){        //here.id as Int ){
					 val result = solver.getIPVector(csp_, totalCost );
					 if (result){
						  nbChangeV++;
						  mark.clear();
						  totalCost = csp_.costOfSolution(true);
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
					 //restartVar(csp_);
					 
					 // get a conf from the Local Min Pool
					 val result = solver.getLM(csp_, totalCost );
					 //Utils.show("new conf: ", csp_.getVariables());
					 if (result){
						  //nbChangeV++;
						  mark.clear();
						  totalCost = csp_.costOfSolution(true);
						  
						  // Different number of moves each time 
						  val resetM = random.nextInt(size / 4n);
						  doReset(resetM , csp_);
						  //doReset(nbVarReset as Int , csp_);
						  bestSent = true;
						  //Console.OUT.println("Changing vector in "+ here);
					 }
					 continue;
				}
				
				if (forceReset){
					 //reset
					 Logger.info(()=>{"   ASSolverPermut : force Reset"});
					 forceReset = false;
					 nbForceRestart++;
					 //doReset(size as Int / 8n , csp_);
					 doReset(nbVarReset as Int , csp_); // This reset should be bigger than the normal reset
					 continue;
				}	
				
				// ----- end of interaction with other solvers -----
		  } // while (totalCost != 0n)
		  
		  nbIterTot += nbIter;
		  nbResetTot += nbReset;	
		  nbSwapTot += nbSwap;
		  nbSameVarTot += nbSameVar;
		  nbLocalMinTot += nbLocalMin; 
		  
		  //csp_.displaySolution();
		  //Loger.info(()=>{"   ASSolverPermut: Finish search with best cost: "+bestCost+" kill="+kill });
		  
		  // if (bestCost == 0n){
		  // 	//Loger.info(()=>{"perfect solution found "});
		  // 	csp_.displaySolution(bestConf as Valuation(sz));
		  // }
		  // else{
		  // 	Logger.debug(()=>{"Best marriage found - BP= "+bestnbBP+" Singles="+bestnbSG});
		  // 	//csp_.displaySolution(bestConf as Valuation(sz));
		  // }
		  
		  
		  //creating Artificial errors for testing purposes
		  //csp_.swapVariables(1n,150n);
		  //csp_.swapVariables(1n,2n);
		  //bestConf(2)=2n;
		  //val tmp = bestConf(2);
		  //bestConf(2)=bestConf(3);
		  //bestConf(3)=tmp;
		  
		  
		  //Rail.copy(csp_.getVariables(),bestConf as Valuation(sz));
		  
		  return bestCost;
	 }
	 
	 /**
	  * 	selectVarHighCost( csp : ModelAS ) : Int
	  * 	Select the maximum cost variable of the problem 
	  *  Also computes the number of marked variables.
	  *  @param csp problem model
	  * 	@return the index of the variable with high individual cost
	  */
	 private def selectVarHighCost( csp_ : ModelAS ) : Int{
		  
		  var i: Int =-1n;
		  var x: Int;
		  var max: Int = 0n;
		  
		  listInb = 0n; //Number of elements
		  nbVarMarked = 0n; 
		  //Console.OUT.println("Aqui");
		  //while(++i < size)  {
		  while((i = csp_.nextI(i)) as UInt < size as UInt) { //False if i < 0
				if (nbSwap < mark(i)) {
					 nbVarMarked++;
					 continue;
				}
				//Console.OUT.println("Aqui");
				x = csp_.costOnVariable(i);
				//Console.OUT.println("var: "+i+" cost= "+x);
				if (x >= max){
					 if (x > max){
						  max = x;
						  listInb = 0n;
					 }
					 listI(listInb++) = i; 
				}
		  }
		  if (listInb == 0n) // all variables are OK but the global cost is > 0 (can occur in SMTI with no BP but singles)
				maxI = random.nextInt(size);
		  else {
				x = random.nextInt(listInb);
				//Console.OUT.println("listInb "+listInb+ " x "+x+" listI(x) "+listI(x));
				maxI = listI(x); //This maxI must be local or only returns the value
		  }
		  nbSameVar += listInb;
		  
		  return maxI;
	 }
	 
	 
	 private def selectVarHighCostEO( csp_ : ModelAS ) : Int{
		  
		  var i: Int =-1n;
		  var x: Int;
		  var max: Int = 0n;
		  
		  listInb = 0n; //Number of elements
		  nbVarMarked = 0n; 
		  while((i = csp_.nextI(i)) as UInt < size as UInt) { //False if i < 0
				if (nbSwap < mark(i)) {
					 nbVarMarked++;
					 continue;
				}
				x = csp_.costOnVariable(i);
				
				//Console.OUT.println("var "+i+" = "+x);
				
				fit(i) = new PairAS(i as Int , x);
				
		  }
		  //[PairAS]
		  RailUtils.sort(fit, cmp);
		  
		  //for (f in fit)
		  //  Console.OUT.println("org "+f.i+" = "+f.j);
		  //sort fitness
		  
		  // check sort
		  // select one with a PDF
		  // put some values on ListI? or 
		  
		  val ma = taupick();
		  maxI = fit(ma).i;
		  
		  //Console.OUT.println("pick "+ma+" maxi "+maxI);
		  
		  // if (listInb == 0n) // all variables are OK but the global cost is > 0 (can occur in SMTI with no BP but singles)
		  //   maxI = random.nextInt(size);
		  // else {
		  //   x = random.nextInt(listInb);
		  //   //Console.OUT.println("listInb "+listInb+ " x "+x+" listI(x) "+listI(x));
		  //   maxI = listI(x); //This maxI must be local or only returns the value
		  // }
		  // nbSameVar += listInb;
		  
		  return maxI;
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
	 
	 /**
	  * 	selectVarMinConflict( csp : ModelAS) : Int
	  * 	Computes swap and selects the minimum of cost if swap
	  * 	@param csp problem model
	  * 	@return the index of the variable with minimum individual cost if swap
	  */
	 private def selectVarMinConflict( csp : ModelAS) : Int {
		  var j: Int;
		  var x: Int;
		  var flagOut:Boolean = false; 
		  var lminJ : Int = -1n;
		  
		  //loop: 
		  do {
				//Console.OUT.println(" --- maxI= "+maxI);
				flagOut = false;
				listJnb = 0n;
				newCost = totalCost;
				j = -1n;
				
				while((j = csp.nextJ(maxI, j, 0n)) as UInt < size as UInt) // false if j < 0 //solverP.exhaustive???
				{	
					 if (nbSwap < mark(j)) {
						  continue;
					 }
					 //Console.OUT.println("swap "+j+"/"+maxI);
					 x = csp.costIfSwap(totalCost, j, maxI);
					 //Console.OUT.println("swap "+j+"/"+maxI+"  Cost= "+x);
					 
					 if (solverP.probSelectLocMin <= 100n && j == maxI) continue;
					 
					 if (x < newCost){
						  listJnb = 1n;
						  newCost = x;
						  lminJ = j;
						  if (solverP.firstBest)
								return lminJ;   
					 } else if (x == newCost){
						  if (random.nextInt(++listJnb) == 0n)
								lminJ = j;
					 }
				}
				
				if (solverP.probSelectLocMin <= 100n) {
					 if (newCost >= totalCost && 
								(random.nextInt(100n) < solverP.probSelectLocMin 
										  ||(listInb <= 1n && listJnb <= 1n))) {
						  lminJ = maxI;
						  return lminJ;
					 }
					 
					 if (listJnb == 0n) {
						  //Console.OUT.println("listInb= "+listInb);
						  nbIter++;
						  x = random.nextInt(listInb);
						  maxI = listI(x);
						  flagOut = true;
					 }
				}
		  } while(flagOut);
		  //Console.OUT.println("list_J = "+ listJnb);
		  return lminJ;
	 }
	 
	 
	 private def select2VarEO( csp : ModelAS) : Int {
		  val j = random.nextInt(size);
		  newCost = csp.costIfSwap(totalCost, j, maxI);	 
		  return j;
	 }
	 
	 /**
	  * 	doReset( var n : Int, csp : ModelAS )
	  * 	Performs the reset over the problem model csp
	  *  @param n number of variables to reset
	  * 	@param csp Model to reset
	  */
	 private def doReset(n:Int, csp_ : ModelAS ) {
		  
		  var cost : Int = -1n;		//reset(n, csp);
		  
		  // if (totalCost < rBestCost){
		  // 	 rBestCost = totalCost;
		  // 	 Console.OUT.println("Loc Min = "+locMinC);
		  // 	 locMinC = 1n;
		  // }else{
		  // 	 locMinC++;
		  // }
		  
		  cost = csp_.reset( n, totalCost );
		  //nbSwap += n ; //I don't know what happened here with costas reset
		  
		  mark.clear();
		  nbReset++;
		  //Console.OUT.println("Do reset...: "+ nbReset);
		  totalCost = (cost < 0n) ? csp_.costOfSolution(true) : cost; //Arg costofsol(1)
	 }
	 
	 // 	public def changeVector(csp : ModelAS){
	 // 		var ipVector : Int = -1;
	 // 		
	 // 		//Main.show("antes= ",csp.variables);
	 // 		ipVector = solverC.getIPVector(csp, totalCost, commRefs);
	 // 		//Main.show("despues= ",csp.variables);
	 // 		
	 // 		if (ipVector == 1){
	 // 			nbChangeV++;
	 // 			nbSwap += size;
	 // 			//Console.OUT.println("do change vector");
	 // 			mark.clear();
	 // 			totalCost = csp.costOfSolution(1); //Arg costofsol(1)
	 // 		}
	 // 
	 // 	}
	  
	 /**
	  *  Computes maxI and minJ, the 2 variables to swap.
	  *  All possible pairs are tested exhaustively.
	  */
	 private def selectVarsToSwap(csp : ModelAS) {
		  var i : Int;
		  var j : Int;
		  var x : Int;
		  
		  nbListIJ = 0n;
		  newCost = x10.lang.Int.MAX_VALUE ;
		  nbVarMarked = 0n;
		  
		  //Console.OUT.println("TC =>"+totalCost);
		  
		  i = -1n;
		  //while(++i < size) { // false if i < 0
		  while((i = csp.nextI(i))as UInt < size as UInt) {
				if ( nbSwap < mark(i) ) {
					 nbVarMarked++;
					 continue;
				}
				//j = i; 
				j = -1n;
				//while(++j < size) {
				while((j = csp.nextJ(i, j, i + 1n ))as UInt < size as UInt ){
					 //while((unsigned) (j = Next_J(i, j, i + 1)) < (unsigned) ad.size) // false if j < 0
					 if ( nbSwap < mark(j) ) {
						  continue;
					 }
					 //Console.OUT.println("SWAP "+i+" <-> "+j);
					 x = csp.costIfSwap(totalCost, i, j);
					 //Console.OUT.println("cost = "+x);
					 
					 if (x <= newCost) {
						  if (x < newCost) {
								newCost = x;
								nbListIJ = 0n;
								if (solverP.firstBest == true && x < totalCost) {
									 maxI = i;
									 minJ = j;
									 return; 
								}
						  }
						  listIJ(nbListIJ) = new PairAS(i,j);
						  //listIJ(nbListIJ).i = i;
						  //listIJ(nbListIJ).j = j;
						  nbListIJ = (nbListIJ + 1n) % size;
					 }
				}
		  }
		  
		  nbSameVar += nbListIJ;
		  
		  if (newCost >= totalCost) {
				if (nbListIJ == 0n || 
						  (( solverP.probSelectLocMin <= 100n) 
									 && random.nextInt(100n) < solverP.probSelectLocMin)) {
					 for(i = 0n; nbSwap < mark(i); i++)
					 {}
					 maxI = minJ = i;
					 return;//goto end;
				}
				
				if (!(solverP.probSelectLocMin <= 100n) 
						  && (x = random.nextInt(nbListIJ + size)) < size) {
					 maxI = minJ = x;
					 return;//goto end;
				}
		  }
		  
		  x = random.nextInt(nbListIJ);
		  maxI = listIJ(x).i;
		  minJ = listIJ(x).j;
		  return;
	 }
	 
	 private def testSelectVarHighCost(csp_: ModelAS){ 
		  var test:Int;
		  
		  csp_.setParameters(solverP);
		  csp_.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
		  
		  mark.clear();
		  csp_.costOfSolution(true);
		  var timeStart :Long = x10.lang.System.nanoTime();
		  test = selectVarHighCost(csp_);
		  var timeEnd :Long = x10.lang.System.nanoTime(); 
		  
		  Console.OUT.println("maxI= "+test);
		  
		  return timeEnd-timeStart;
	 }
	 
	 /**
	  * 	Clear function
	  */
	 public def clear(){
		  this.kill = false;
	 }
	 
	 public def forceRestart(){
		  Logger.info(()=>"ASSolverPermut: Force Restart True");
		  forceRestart = true;
	 }
	 public def forceReset(){
		  Logger.info(()=>"ASSolverPermut: Force Reset True");
		  forceReset = true;
	 }
	 
	 public def restartVar(csp : ModelAS){
		  //Logger.info(()=>"ASSolver Permut: Restart");
		  csp.initialize(solverP.baseValue); // Random Permut
		  totalCost = csp.costOfSolution(true);
		  bestOfBest = x10.lang.Int.MAX_VALUE ;
		  Rail.copy(csp.getVariables() as Valuation(sz),bestConf as Valuation(sz));
		  bestCost = totalCost;
		  bestSent = false;
		  nbInPlateau = 0n;
		  
		  //Not sure if this is necessary
		  solver.clearIntPool();//??? Restart only the pool		
		  
		  mark.clear();
		  //nbRestart++;			
		  //Update Total statistics
		  nbIterTot += nbIter;
		  nbResetTot += nbReset;        
		  nbSwapTot += nbSwap;
		  nbSameVarTot += nbSameVar;
		  nbLocalMinTot += nbLocalMin; 
		  //Restart local var
		  nbSwap = 0n;
		  nbIter = 0n;
		  nbSameVar = 0n;
		  nbLocalMin = 0n;
		  nbReset = 0n;
	 }
	 
	 /**
	  * 	Return the aray of variables withe the best cost
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
public type ASSolverPermut(s:Long) = ASSolverPermut{self.sz==s};