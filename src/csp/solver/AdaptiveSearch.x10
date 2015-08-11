package csp.solver;
import x10.util.StringUtil;
import csp.model.ModelAS;
import csp.util.Logger;
import csp.model.ParamManager;

/**
 * Class AdaptiveSearch
 */
public class AdaptiveSearch extends RandomSearch {
	 
	 private val mark : Rail[Int]; 
	 //private var maxI : Int;		
	 //private var minJ : Int;
	 
	 private var listInb : Int;
	 private var listJnb : Int;
	 
	 private val listIJ : Rail[MovePermutation];
	 private val listI : Rail[Long];
	 
	 private var nVarMarked : Int = 0n; 
	 
	 /**	Statistics	*/
	 private var nForceRestart : Int = 0n;
	 private var nReset : Int;
	 private var nSameVar : Int;
	 private var nLocalMin : Int;
	 /** Number time to change vector due to communication */ 
	 private var nChangeV : Int = 0n;
	 
	 /** Total Statistics */
	 private var nResetTot : Int;	
	 private var nSameVarTot : Int;
	 private var nLocalMinTot : Int; 
	 
	 private var nInPlateau:Int; 
	 
	 /** For Exhaustive search */
	 private var nListIJ : Int;
	 
	 private solver:IParallelSolver(sz); 
	 
	 private var pSendLM:Double = 0.5;
	 
	 private var bestSent:Boolean=false;
	 
	 /** Parameters of the AS solver */
	 private var nVarToReset:Long;
	 private var resetPercent:Int;
	 private val exhaustive:Boolean; 
	 private val freezeLocMin:Int;
	 private val freezeSwap:Int;
	 private val resetLimit:Int;
	 private val probSelectLocMin:Int;
	 private val firstBest:Boolean;
	 
	 
	 public def this(sz:Long, solver:IParallelSolver(sz), opts:ParamManager){
		  super(sz, opts);
		  this.mark = new Rail[Int] (this.sz, 0n);
		  this.listIJ = new Rail[MovePermutation](this.sz);
		  this.listI = new Rail[Long](this.sz, 0);
		  this.solver = solver;
		  
		  val str = System.getenv("LM");
		  if (str != null)
				pSendLM = StringUtil.parseInt(str)/ 100.0;
		  
		  // Set parameters from the ParamManager object
		  this.nVarToReset = opts("--AS_varToReset",-1);
		  this.resetPercent = opts("--AS_resetPer",10n);
		  this.freezeLocMin = opts("--AS_freezeLocMin",5n);
		  this.freezeSwap = opts("--AS_freezeSwap",5n);
		  this.resetLimit = opts("--AS_resetLimit",5n);
		  this.probSelectLocMin = opts("--AS_probSelecLocMin", 0n);
		  this.firstBest = opts("--AS_firstBest",0n) == 1n;
		  this.exhaustive = opts("--AS_exhaustive",0n) == 1n;
		  
		  if (here.id == 0)
				Console.OUT.println("Parameters AS: nVarToReset="+nVarToReset+" resetPercent="+resetPercent+
					 " freezeLocMin="+freezeLocMin+" freezeSwap="+freezeSwap+" resetLimit="+resetLimit+
					 " probSelectLocMin="+probSelectLocMin+" exhaustive="+exhaustive+" firstBest="+firstBest);
	 }
	 
	 
	 protected def initVar( cop_:ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  super.initVar(cop_, tCost, sLow);
		  
		  Logger.debug(()=>{"ASSolver"});
		  
		  if (this.nVarToReset == -1){
				this.nVarToReset = (((this.sz * resetPercent as Long) + 99) / 100);
				if (this.nVarToReset < 2){
					 this.nVarToReset = 2;
					 Logger.debug(()=>{"increasing nb var to reset since too small, now = "+ this.nVarToReset});
				}
		  }
		  
		  
		  val nStr = System.getenv("N");
		  val nbVarReset = (nStr==null)? 0 : StringUtil.parseLong(nStr);
		  
		  mark.clear();
		  listI.clear();
		  
		  nRestart = 0n;
		  nSameVar = 0n;
		  nLocalMin = 0n;
		  nReset = 0n;
		  nChangeV = 0n;
		  nInPlateau = 0n;
		  
		  nResetTot = 0n;	
		  nSameVarTot = 0n;
		  nLocalMinTot = 0n; 
		  nForceRestart = 0n;
		  bestSent = false;
	 }
	 
	 /**
	  *  Search process (in loop functionality)
	  *  To be overwrited for each child class (solver) 
	  */
	 protected def search( cop_ : ModelAS{self.sz==this.sz}) : Long{
		  var newCost:Long = -1;
		  
		  
		  if( !this.exhaustive ){
				//Console.OUT.println("No exhaustive");
				selectVarHighCost( cop_ , move );
				newCost = selectVarMinConflict( cop_, move );
				
		  } else {
				//Console.OUT.println("exhaustive");
				
				newCost = selectVarsToSwap( cop_, move );
		  }
		  //Console.OUT.println("----- iter no: "+nIter+", cost: "+currentCost+", n marked: "+nVarMarked+" ---, n_swap= "+nSwap);
		  
		  if (currentCost != newCost) {
				if (nInPlateau > 1n) {
					 //Console.OUT.println("end of plateau, length: "+ nbInPlateau);
				}
				nInPlateau = 0n;
		  }
		  //if (newCost < bestCost) bestCost = newCost;
		  
		  nInPlateau++;
		  
		  //if (minJ == -1n) continue;
		  
		  if (move.getFirst() == move.getSecond()) {
				this.nLocalMin++;
				mark(move.getFirst()) = this.nSwap + this.freezeLocMin; //Mark(maxI, freeze_loc_min);
				//Console.OUT.println("nVarMarked "+nVarMarked+"resetLimit= "+solverP.resetLimit);
				if (this.nVarMarked + 1 >= this.resetLimit)
				{				
					 // communicate Local Minimum
					 if (random.nextDouble() < pSendLM)
						  solver.communicateLM( this.currentCost, cop_.getVariables());
					 
					 doReset(this.nVarToReset, cop_);//doReset(nb_var_to_reset,csp);
					 //Utils.show("after reset= ",csp_.getVariables());
				}
		  }else {
				mark(move.getFirst()) = this.nSwap + this.freezeSwap; //Mark(maxI, ad.freeze_swap);
				mark(move.getSecond()) = this.nSwap + this.freezeSwap; //Mark(minJ, ad.freeze_swap);
				cop_.swapVariables(move.getFirst(), move.getSecond()); //adSwap(maxI, minJ,csp);
				nSwap++;
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
		  nResetTot += nReset;        
		  nSameVarTot += nSameVar;
		  nLocalMinTot += nLocalMin; 
		  //Restart local var
		  nSameVar = 0n;
		  nLocalMin = 0n;
		  nReset = 0n;
	 }
	 
	 private def doReset(n:Long, cop_ : ModelAS ) {
		  var cost : Long = -1;		//reset(n, csp);
		  cost = cop_.reset( n, currentCost );
		  mark.clear();
		  nReset++;
		  //Console.OUT.println("Do reset...: "+ nbReset);
		  currentCost = (cost < 0) ? cop_.costOfSolution(true) : cost; //Arg costofsol(1)
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
		  var i: Long =-1;
		  var maxCost: Long = 0;
		  var maxVar:Long = -1;
		  
		  this.listInb = 0n; //Number of elements
		  this.nVarMarked = 0n; 
		  
		  //Console.OUT.println("sz="+this.sz);
		  
		  while((i = cop_.nextI(i))as ULong < this.sz as ULong) { //False if i < 0
		  //for(i = 0; i < this.sz; i++ ){
				//Console.OUT.println("i="+i);
				if (this.nSwap < this.mark(i)) {
					 this.nVarMarked++;
					 //Console.OUT.println("nVar="+this.nVarMarked);
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
				maxVar = random.nextLong(this.sz);
		  else {
		      // select a maxCost variable from array
				val sel = random.nextInt(this.listInb);
				//Console.OUT.println("listInb "+listInb+ " x "+x+" listI(x) "+listI(x));
				maxVar = this.listI(sel); //This maxI must be local or only returns the value
		  }
		  this.nSameVar += this.listInb;
		  
		  move.setFirst(maxVar);
	 }
	  	 
	 /**
	  * 	selectVarMinConflict( csp : ModelAS) : Int
	  * 	Computes swap and selects the minimum of cost if swap
	  * 	@param csp problem model
	  * 	@param move object (permutation)
	  * 	@return new cost of the possible move
	  */
	 private def selectVarMinConflict( cop : ModelAS, move:MovePermutation ) : Long {
		  var j: Long;
		  var cost: Long;
		  var flagOut:Boolean = false; 
		  var second : Long = -1;
		  var nCost:Long;
		  var first:Long = move.getFirst();
		  
		  do {
				flagOut = false;
				this.listJnb = 0n;
				nCost = this.currentCost;
				j = -1n;
				
				while((j = cop.nextJ(first, j, false)) as ULong < this.sz as ULong) // false if j < 0 //solverP.exhaustive???
				{	
					 if (this.nSwap < this.mark(j)) {
						  continue;
					 }
					 //Console.OUT.println("swap "+j+"/"+maxI);
					 cost = cop.costIfSwap(this.currentCost, j, first);
					 //Console.OUT.println("swap "+j+"/"+maxI+"  Cost= "+x);
					 
					 if (this.probSelectLocMin <= 100n && j == first) continue;
					 
					 if (cost < nCost){
						  this.listJnb = 1n;
						  nCost = cost;
						  second = j;
						  if (this.firstBest){
								move.setSecond(second);
								return nCost;
						  }
					 } else if (cost == nCost){
						  if (random.nextInt(++this.listJnb) == 0n)
								second = j;
					 }
				}
				
				if (this.probSelectLocMin <= 100n) {
					 if (nCost >= this.currentCost && 
								(random.nextInt(100n) < this.probSelectLocMin 
										  ||(this.listInb <= 1n && this.listJnb <= 1n))) {
						  second = first;
						  move.setSecond(second);
						  return nCost;
					 }
					 
					 if (this.listJnb == 0n) {
						  
						  // TODO: Do this only once!!!
						  //Console.OUT.println("listInb= "+listInb);
						  this.nIter++;
						  val sel = random.nextInt(this.listInb);
						  first = listI(sel);
						  move.setFirst(first);
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
	 private def selectVarsToSwap(cop : ModelAS, move:MovePermutation):Long {
		  var first : Long;
		  var second : Long;
		  //var x : Int;
		  
		  this.nListIJ = 0n;
		  var nCost:Long = Long.MAX_VALUE ;
		  this.nVarMarked = 0n;
		  
		  //Console.OUT.println("TC =>"+totalCost);
		  
		  first = -1n;
		  //while(++i < sz) { // false if i < 0
		  while((first = cop.nextI(first))as ULong < this.sz as ULong) {
				if ( this.nSwap < this.mark(first) ) {
					 this.nVarMarked++;
					 continue;
				}
				//j = i; 
				second = -1n;
				//while(++j < sz) {
				while((second = cop.nextJ(first, second, true))as ULong < this.sz as ULong ){
					 if ( this.nSwap < this.mark(second) ) {
						  continue;
					 }
					 //Console.OUT.println("SWAP "+i+" <-> "+j);
					 val x = cop.costIfSwap(currentCost, first, second);
					 //Console.OUT.println("cost = "+x);
					 
					 if (x <= nCost) {
						  if (x < nCost) {
								nCost = x;
								this.nListIJ = 0n;
								if (this.firstBest == true && x < this.currentCost) {
									 move.setFirst(first);
									 move.setSecond(second);
									 return nCost; 
								}
						  }
						  this.listIJ(this.nListIJ) = new MovePermutation(first,second);
						  this.nListIJ = ((this.nListIJ + 1) % this.sz) as Int;
					 }
				}
		  }
		  
		  this.nSameVar += this.nListIJ;
		  
		  if (nCost >= this.currentCost) {
				if (this.nListIJ == 0n || 
						  (( this.probSelectLocMin <= 100n) 
									 && random.nextInt(100n) < this.probSelectLocMin)) {
					 var i:Int;
					 for(i = 0n; this.nSwap < mark(i); i++)
					 {}
					 move.setFirst(i);
					 move.setSecond(i);
					 return nCost;//goto end;
				}
				
				var lm:Long;
				if (!(this.probSelectLocMin <= 100n) 
						  && (lm = random.nextLong(this.nListIJ + this.sz)) < this.sz) {
					 move.setFirst(lm);
					 move.setSecond(lm);
					 return nCost;//goto end;
				}
		  }
		  
		  val sel = random.nextInt(this.nListIJ);
		  move.setFirst(listIJ(sel).getFirst());
		  move.setSecond(listIJ(sel).getSecond());
		  return nCost;
	 }
	 
	 
	 
	 /**
	  *  Interact with other entities
	  */
	 protected def interact( cop_:ModelAS{self.sz==this.sz}){
		  //Console.OUT.println("AS interact");
		  
		  /**
		   *  Interaction with other places
		   */
		  if( solver.inTeamReportI() != 0n && nIter % solver.inTeamReportI() == 0n){  //here.id as Int ){
				//Console.OUT.println("report");
				if(!bestSent){ 
					 solver.communicate( this.bestCost, this.bestConf as Valuation(sz));
					 bestSent = true;
				}else{
					 solver.communicate( this.currentCost, cop_.getVariables());
				}
		  }
		  
		  if(solver.inTeamUpdateI() != 0n && this.nIter % solver.inTeamUpdateI() == 0n){        //here.id as Int ){
				//Console.OUT.println("update");
				val result = solver.getIPVector(cop_, this.currentCost );
				if (result){
					 this.nChangeV++;
					 this.mark.clear();
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
				//restartVar(csp_);
				
				val result = this.solver.getLM(cop_, this.currentCost );
				//Utils.show("new conf: ", csp_.getVariables());
				if (result){
					 //nbChangeV++;
					 this.mark.clear();
					 this.currentCost = cop_.costOfSolution(true);
					 this.doReset(nVarToReset, cop_);
					 this.bestSent = true;
					 //Console.OUT.println("Changing vector in "+ here);
				}
				
				// get a conf from the Local Min Pool

				
				
				
				//restart
				// Logger.info(()=>{"   ASSolverPermut : force Restart"});
				// this.forceRestart = false;
				// this.nForceRestart++;
				// this.restartVar(cop_);
		  }
		  
		  if (this.forceReset){
				//reset
				Logger.info(()=>{"   ASSolverPermut : force Reset"});
				this.forceReset = false;
				this.nForceRestart++;
				//doReset(size as Int / 8n , csp_);
				this.doReset(this.nVarToReset , cop_); // This reset should be bigger than the normal reset
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
	 
	 /**
	  * 	Report statistics from the solving process
	  */
	 public def reportStats( c : CSPStats){
		  super.reportStats(c);
		  c.locmin = this.nLocalMinTot;
		  c.reset = this.nResetTot;
		  c.same = this.nSameVarTot;
		  c.change = this.nChangeV;
		  c.forceRestart = this.nForceRestart;
	 }
	 
	 protected def updateTotStats(){
		  super.updateTotStats();
		  nResetTot += nReset;
		  nSameVarTot += nSameVar;
		  nLocalMinTot += nLocalMin;
	 }
	 
	 
}
public type AdaptiveSearch(s:Long)=AdaptiveSearch{self.sz==s};