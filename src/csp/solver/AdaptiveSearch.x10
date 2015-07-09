package csp.solver;
import csp.model.ASSolverParameters;
import x10.util.StringUtil;
import csp.model.ModelAS;
import csp.util.Logger;

/**
 * Class AdaptiveSearch
 */
public class AdaptiveSearch extends RandomSearch {
	 
	 private val mark : Rail[Int]; 
	 private val solverP = new ASSolverParameters();
	 
	 //private var maxI : Int;		
	 //private var minJ : Int;
	 
	 private var listInb : Int;
	 private var listJnb : Int;
	 
	 private val listIJ : Rail[MovePermutation];
	 private val listI : Rail[Int];
	 
	 private var nbVarMarked : Int = 0n; 
	 
	 /**	Statistics	*/
	 private var nbRestart : Int = 0n;
	 private var nbForceRestart : Int = 0n;
	 private var nbReset : Int;
	 private var nbSameVar : Int;
	 private var nbLocalMin : Int;
	 /** Number time to change vector due to communication */ 
	 private var nbChangeV : Int = 0n;
	 
	 /** Total Statistics */
	 private var nbResetTot : Int;	
	 private var nbSameVarTot : Int;
	 private var nbLocalMinTot : Int; 
	 
	 private var nbInPlateau:Int; 
	 
	 /** For Exhaustive search */
	 private var nbListIJ : Int;
	 
	 private solver:IParallelSolver(sz); 
	 
	 private var pSendLM:Double = 0.5;
	 
	 private var bestSent:Boolean=false;
	 
	 public def this(sz:Long, size:Int, solver:IParallelSolver(sz), mTime:Long){
		  super(sz, size, mTime);
		  this.mark = new Rail[Int] (this.size, 0n);
		  this.listIJ = new Rail[MovePermutation](this.size);
		  this.listI = new Rail[Int](this.size, 0n);
		  this.solver = solver;
		  
		  val str = System.getenv("LM");
		  if (str != null)
				pSendLM = StringUtil.parseInt(str)/ 100.0;
	 }
	 
	 
	 protected def initVar( cop_:ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  super.initVar(cop_, tCost, sLow);
		  
		  Logger.debug(()=>{"ASSolver"});
		  cop_.setParameters(solverP);
		  
		  if (solverP.nbVarToReset == -1n){
				solverP.nbVarToReset = (((size * solverP.resetPercent) + (100n) - 1n) / (100n));
				if (solverP.nbVarToReset < 2n){
					 solverP.nbVarToReset = 2n;
					 Logger.debug(()=>{"increasing nb var to reset since too small, now = "+ solverP.nbVarToReset});
				}
		  }
		  
		  
		  val nStr = System.getenv("N");
		  val nbVarReset = (nStr==null)? 0 : StringUtil.parseLong(nStr);
		  
		  mark.clear();
		  listI.clear();
		  
		  nbRestart = 0n;
		  nbSameVar = 0n;
		  nbLocalMin = 0n;
		  nbReset = 0n;
		  nbChangeV = 0n;
		  nbInPlateau = 0n;
		  
		  nbResetTot = 0n;	
		  nbSameVarTot = 0n;
		  nbLocalMinTot = 0n; 
		  nbForceRestart = 0n;
		  bestSent = false;
	 }
	 
	 /**
	  *  Search process (in loop functionality)
	  *  To be overwrited for each child class (solver) 
	  */
	 protected def search( cop_ : ModelAS{self.sz==this.sz}) : Int{
		  
		  var newCost:Int = -1n;
		  
		  if (this.nbIter >= solverP.restartLimit){
				if(this.nbRestart < solverP.restartMax){
					 //restart
					 //forceRestartalse;
					 this.restart = true;
					 return this.currentCost;	 
				}
				//Console.OUT.println("Not solution found");
				// End solving process
				this.kill = true;
				return this.currentCost;
		  }
		  
		  if( !solverP.exhaustive ){
				selectVarHighCost( cop_ , move );
				newCost = selectVarMinConflict( cop_, move );
				//Console.OUT.println("maxI "+maxI+"  minJ "+minJ+" cost "+totalCost+"  newCost "+newCost);
		  } else {
				newCost = selectVarsToSwap( cop_, move );
				//Console.OUT.println("maxI= "+maxI+"  minJ= "+minJ);
		  }
		  //Logger.debug(()=>{"----- iter no: "+nbIter+", cost: "+totalCost+", nb marked: "+nbVarMarked+" ---, nb_swap= "+nbSwap});
		  //Console.OUT.println("----- iter no: "+nbIter+", cost: "+totalCost+", nb marked: "+nbVarMarked+" ---, nb_swap= "+nbSwap);
		  
		  if (currentCost != newCost) {
				if (nbInPlateau > 1n) {
					 //Console.OUT.println("end of plateau, length: "+ nbInPlateau);
				}
				nbInPlateau = 0n;
		  }
		  //if (newCost < bestCost) bestCost = newCost;
		  
		  nbInPlateau++;
		  
		  //if (minJ == -1n) continue;
		  
		  if (move.getFirst() == move.getSecond()) {
				this.nbLocalMin++;
				mark(move.getFirst()) = this.nbSwap + solverP.freezeLocMin; //Mark(maxI, freeze_loc_min);
				//Console.OUT.println("nbVarMarked "+nbVarMarked+"solverP.resetLimit= "+solverP.resetLimit);
				if (this.nbVarMarked + 1 >= solverP.resetLimit)
				{				
					 // communicate Local Minimum
					// if (random.nextDouble() < pSendLM)
						  solver.communicateLM( this.currentCost, cop_.getVariables());
					 
					 doReset(solverP.nbVarToReset, cop_);//doReset(nb_var_to_reset,csp);
					 //Utils.show("after reset= ",csp_.getVariables());
				}
		  }else {
				mark(move.getFirst()) = this.nbSwap + solverP.freezeSwap; //Mark(maxI, ad.freeze_swap);
				mark(move.getSecond()) = this.nbSwap + solverP.freezeSwap; //Mark(minJ, ad.freeze_swap);
				cop_.swapVariables(move.getFirst(), move.getSecond()); //adSwap(maxI, minJ,csp);
				nbSwap++;
				cop_.executedSwap(move.getFirst(), move.getSecond());
				this.currentCost = newCost;
				//Console.OUT.println("swap "+maxI+" <-> "+minJ);
		  }
		  return currentCost;
	 }
	 
	 protected def restartVar(cop : ModelAS){
		  super.restartVar(cop);
		  //Rail.copy(csp.getVariables() as Valuation(sz),bestConf as Valuation(sz));
		  //bestCost = totalCost;
		  bestSent = false;
		  mark.clear();
		  //nbRestart++;			
		  //Update Total statistics
		  nbResetTot += nbReset;        
		  nbSameVarTot += nbSameVar;
		  nbLocalMinTot += nbLocalMin; 
		  //Restart local var
		  nbSameVar = 0n;
		  nbLocalMin = 0n;
		  nbReset = 0n;
	 }
	 
	 private def doReset(n:Int, cop_ : ModelAS ) {
		  var cost : Int = -1n;		//reset(n, csp);
		  cost = cop_.reset( n, currentCost );
		  mark.clear();
		  nbReset++;
		  //Console.OUT.println("Do reset...: "+ nbReset);
		  currentCost = (cost < 0n) ? cop_.costOfSolution(true) : cost; //Arg costofsol(1)
	 }
	 
	 
	 /**
	  * 	SelectVarHighCost 
	  * 	Select the maximum cost variable of the problem 
	  *   (Modify the first variable of the move object )
	  *   Also computes the number of marked variables.
	  *   
	  *   @param csp problem model
	  * 	@param move object (permutation)
	  */
	 private def selectVarHighCost( cop_ : ModelAS , move:MovePermutation){
		  var i: Int =-1n;
		  var maxCost: Int = 0n;
		  var maxVar:Int = -1n;
		  
		  this.listInb = 0n; //Number of elements
		  this.nbVarMarked = 0n; 
		  while((i = cop_.nextI(i)) as UInt < this.size as UInt) { //False if i < 0
				if (this.nbSwap < this.mark(i)) {
					 this.nbVarMarked++;
					 continue;
				}
				val x = cop_.costOnVariable(i);
				//Console.OUT.println("var: "+i+" cost= "+x);
				if (x >= maxCost){
					 if (x > maxCost){
						  maxCost = x;
						  this.listInb = 0n;
					 }
					 this.listI(this.listInb++) = i; 
				}
		  }
		  if (this.listInb == 0n) // all variables are OK but the global cost is > 0 (can occur in SMTI with no BP but singles)
				maxVar = random.nextInt(this.size);
		  else {
		      // select a maxCost variable from array
				val sel = random.nextInt(this.listInb);
				//Console.OUT.println("listInb "+listInb+ " x "+x+" listI(x) "+listI(x));
				maxVar = this.listI(sel); //This maxI must be local or only returns the value
		  }
		  this.nbSameVar += this.listInb;
		  
		  move.setFirst(maxVar);
	 }
	  	 
	 /**
	  * 	selectVarMinConflict( csp : ModelAS) : Int
	  * 	Computes swap and selects the minimum of cost if swap
	  * 	@param csp problem model
	  * 	@param move object (permutation)
	  * 	@return new cost of the possible move
	  */
	 private def selectVarMinConflict( cop : ModelAS, move:MovePermutation ) : Int {
		  var j: Int;
		  var cost: Int;
		  var flagOut:Boolean = false; 
		  var second : Int = -1n;
		  var nCost:Int;
		  var first:Int = move.getFirst();
		  
		  do {
				flagOut = false;
				this.listJnb = 0n;
				nCost = this.currentCost;
				j = -1n;
				
				while((j = cop.nextJ(first, j, 0n)) as UInt < this.size as UInt) // false if j < 0 //solverP.exhaustive???
				{	
					 if (this.nbSwap < this.mark(j)) {
						  continue;
					 }
					 //Console.OUT.println("swap "+j+"/"+maxI);
					 cost = cop.costIfSwap(this.currentCost, j, first);
					 //Console.OUT.println("swap "+j+"/"+maxI+"  Cost= "+x);
					 
					 if (solverP.probSelectLocMin <= 100n && j == first) continue;
					 
					 if (cost < nCost){
						  this.listJnb = 1n;
						  nCost = cost;
						  second = j;
						  if (solverP.firstBest){
								move.setSecond(second);
								return nCost;
						  }
					 } else if (cost == nCost){
						  if (random.nextInt(++this.listJnb) == 0n)
								second = j;
					 }
				}
				
				if (solverP.probSelectLocMin <= 100n) {
					 if (nCost >= this.currentCost && 
								(random.nextInt(100n) < solverP.probSelectLocMin 
										  ||(this.listInb <= 1n && this.listJnb <= 1n))) {
						  second = first;
						  move.setSecond(second);
						  return nCost;
					 }
					 
					 if (this.listJnb == 0n) {
						  //Console.OUT.println("listInb= "+listInb);
						  this.nbIter++;
						  val sel = random.nextInt(this.listInb);
						  second = listI(sel);
						  flagOut = true;
					 }
				}
		  } while(flagOut);
		  move.setSecond(second);
		  return nCost;
	 }
	 
	 /**
	  *  Computes maxI and minJ, the 2 variables to swap.
	  *  All possible pairs are tested exhaustively.
	  *  @param csp problem model
	  *  @param move object (permutation)
	  *  @return new cost of the possible move
	  */
	 private def selectVarsToSwap(cop : ModelAS, move:MovePermutation):Int {
		  var first : Int;
		  var second : Int;
		  //var x : Int;
		  
		  this.nbListIJ = 0n;
		  var nCost: Int = x10.lang.Int.MAX_VALUE ;
		  this.nbVarMarked = 0n;
		  
		  //Console.OUT.println("TC =>"+totalCost);
		  
		  first = -1n;
		  //while(++i < size) { // false if i < 0
		  while((first = cop.nextI(first))as UInt < this.size as UInt) {
				if ( this.nbSwap < this.mark(first) ) {
					 this.nbVarMarked++;
					 continue;
				}
				//j = i; 
				second = -1n;
				//while(++j < size) {
				while((second = cop.nextJ(first, second, first + 1n ))as UInt < this.size as UInt ){
					 if ( this.nbSwap < this.mark(second) ) {
						  continue;
					 }
					 //Console.OUT.println("SWAP "+i+" <-> "+j);
					 val x = cop.costIfSwap(currentCost, first, second);
					 //Console.OUT.println("cost = "+x);
					 
					 if (x <= nCost) {
						  if (x < nCost) {
								nCost = x;
								this.nbListIJ = 0n;
								if (solverP.firstBest == true && x < this.currentCost) {
									 move.setFirst(first);
									 move.setSecond(second);
									 return nCost; 
								}
						  }
						  this.listIJ(this.nbListIJ) = new MovePermutation(first,second);
						  this.nbListIJ = (this.nbListIJ + 1n) % this.size;
					 }
				}
		  }
		  
		  this.nbSameVar += this.nbListIJ;
		  
		  if (nCost >= this.currentCost) {
				if (this.nbListIJ == 0n || 
						  (( solverP.probSelectLocMin <= 100n) 
									 && random.nextInt(100n) < solverP.probSelectLocMin)) {
					 var i:Int;
					 for(i = 0n; this.nbSwap < mark(i); i++)
					 {}
					 move.setFirst(i);
					 move.setSecond(i);
					 return nCost;//goto end;
				}
				
				var lm:Int;
				if (!(solverP.probSelectLocMin <= 100n) 
						  && (lm = random.nextInt(this.nbListIJ + this.size)) < this.size) {
					 move.setFirst(lm);
					 move.setSecond(lm);
					 return nCost;//goto end;
				}
		  }
		  
		  val sel = random.nextInt(this.nbListIJ);
		  move.setFirst(listIJ(sel).getFirst());
		  move.setSecond(listIJ(sel).getSecond());
		  return nCost;
	 }
	 
	 
	 
	 /**
	  *  Interact with other entities
	  */
	 protected def interact( cop_:ModelAS{self.sz==this.sz}){
		  // To be implemented  
		  /**
		   *  Interaction with other places
		   */
		  if( solver.intraTISend() != 0n && nbIter % solver.intraTISend() == 0n){        //here.id as Int ){
				if(!bestSent){ 
					 solver.communicate( this.bestCost, this.bestConf as Valuation(sz));
					 bestSent = true;
				}else{
					 solver.communicate( this.currentCost, cop_.getVariables());
				}
		  }
		  
		  if(solver.intraTIRecv() != 0n && this.nbIter % solver.intraTIRecv() == 0n){        //here.id as Int ){
				val result = solver.getIPVector(cop_, this.currentCost );
				if (result){
					 this.nbChangeV++;
					 this.mark.clear();
					 this.currentCost = cop_.costOfSolution(true);
					 bestSent = true;
					 //Console.OUT.println("Changing vector in "+ here);
				}
		  }
	 }	
	 
	 /**
	  *  Update the cost for the optimization variables
	  *  Reimplemente here to include communication flag "best send"
	  */
	 protected def updateCosts(cop : ModelAS){
		  if(this.currentCost < this.bestCost){ //(totalCost <= bestCost)
				Rail.copy(cop.getVariables(), this.bestConf as Valuation(sz));
				this.bestCost = this.currentCost;
				
				bestSent = false; // new best found, I must send it!
				
				// Console.OUT.println(here+" best cost= "+bestCost);
				// Compare cost and break if target is accomplished
				if ((this.strictLow && this.bestCost < this.target)
						  ||(!this.strictLow && this.bestCost <= this.target)){
					 this.targetSucc = true;
					 this.kill = true;
				}
		  }
	 }
	 
}
public type AdaptiveSearch(s:Long)=AdaptiveSearch{self.sz==s};