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
import x10.util.StringUtil;
import csp.model.GenericModel;
import csp.util.Logger;
import csp.model.ParamManager;
import csp.util.Utils;
import csp.model.Main;

/**
 * Class AdaptiveSearch
 */
public class AdaptiveSearch extends RandomSearch {
	 
	 private val mark : Rail[Int]; 
	 
	 private var listInb : Int;
	 private var listJnb : Int;
	 
	 private val listIJ : Rail[MovePermutation];
	 private val listI : Rail[Long];
	 
	 private var nVarMarked : Int = 0n; 
	 
	 /**	Statistics	*/
	 private var nReset : Int;
	 private var nSameVar : Int;
	 private var nLocalMin : Int;
	 
	 /** Total Statistics */
	 private var nResetTot : Int;	
	 private var nSameVarTot : Int;
	 private var nLocalMinTot : Int; 
	 
	 private var nInPlateau:Int; 
	 
	 /** For Exhaustive search */
	 private var nListIJ : Int;
	 	 
	 /** Parameters of the AS solver */
	 private var nVarToReset:Long;
	 private var resetPercent:Int;
	 private val exhaustive:Boolean; 
	 private val freezeLocMin:Int;
	 private val freezeSwap:Int;
	 private val resetLimit:Int;
	 private val probSelectLocMin:Int;
	 private val firstBest:Boolean;
	 
	 public def this(sizeS:Long, solver:IParallelSolver(sizeS), opts:ParamManager)
	 :AdaptiveSearch(sizeS){
		  super(sizeS, solver, opts);
		  
		  this.mySolverType = Main.AS_SOL;
		  
		  //Console.OUT.println(here+" AS");
		  
		  this.mark = new Rail[Int] (sizeS, 0n);
		  this.listIJ = new Rail[MovePermutation](sizeS);
		  this.listI = new Rail[Long](sizeS, 0);
		  
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
	 
	 
	 protected def initVar( cop_:GenericModel{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  super.initVar(cop_, tCost, sLow);
		  
		  Logger.debug(()=>{"ASSolver"});
		  
		  if (this.nVarToReset == -1){
				this.nVarToReset = (((this.sz * resetPercent as Long) + 99) / 100);
				if (this.nVarToReset < 2){
					 this.nVarToReset = 2;
					 Logger.debug(()=>{"increasing nb var to reset since too small, now = "+ this.nVarToReset});
				}
		  }
		  
		  mark.clear();
		  listI.clear();
		  
		  nRestart = 0n;
		  nSameVar = 0n;
		  nLocalMin = 0n;
		  nReset = 0n;
		  nInPlateau = 0n;
		  
		  nResetTot = 0n;	
		  nSameVarTot = 0n;
		  nLocalMinTot = 0n; 
		  
		 
	 }
	 
	 /**
	  *  Search process (in loop functionality)
	  *  To be overwrited for each child class (solver) 
	  */
	 protected def search( cop_ : GenericModel{self.sz==this.sz}) : Long{
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
					 onLocMin(cop_);
					 
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
	 
	 protected def restartVar(){
		  super.restartVar();
		  mark.clear();
	 }
	 
	 private def doReset(n:Long, cop_ : GenericModel ) {
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
	 private def selectVarHighCost( cop_ : GenericModel , move:MovePermutation){
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
	 private def selectVarMinConflict( cop : GenericModel, move:MovePermutation ) : Long {
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
	 private def selectVarsToSwap(cop : GenericModel, move:MovePermutation):Long {
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
	 //  protected def interact( cop_:ModelAS{self.sz==this.sz}){
	 // 
	 //  }	
	 
	 /**
	  *  Update the cost for the optimization variables
	  *  Reimplemente here to include communication flag "best send"
	  */
	 // protected def updateCosts(cop : ModelAS){
	 //   if(this.currentCost < this.bestCost){ //(totalCost <= bestCost)
	 // 		Rail.copy(cop.getVariables(), this.bestConf as Valuation(sz));
	 // 		this.bestCost = this.currentCost;
	 // 		
	 // 		bestSent = false; // new best found, I must send it!
	 // 		
	 // 		if (this.reportPart){
	 // 			 val eT = (System.nanoTime() - initialTime)/1e9;
	 // 			 val gap = (this.bestCost-this.target)/(this.bestCost as Double)*100.0;
	 // 			 Console.OUT.printf("%s\ttime: %5.1f s\tbest cost: %10d\tgap: %5.2f%% \n",here,eT,this.bestCost,gap);
	 // 		}
	 // 		
	 // 		// Console.OUT.println(here+" best cost= "+bestCost);
	 // 		// Compare cost and break if target is accomplished
	 // 		if ((this.strictLow && this.bestCost < this.target)
	 // 				  ||(!this.strictLow && this.bestCost <= this.target)){
	 // 			 this.targetSucc = true;
	 // 			 this.kill = true;
	 // 		}
	 //   }
	 // }
	 
	 /**
	  * 	Report statistics from the solving process
	  */
	 public def reportStats( c : GlobalStats){
		  super.reportStats(c);
		  c.locmin = this.nLocalMinTot;
		  c.reset = this.nResetTot;
		  c.same = this.nSameVarTot;
	 }
	 
	 protected def updateTotStats(){
		  super.updateTotStats();
		  nResetTot += nReset;
		  nSameVarTot += nSameVar;
		  nLocalMinTot += nLocalMin;
		  nSameVar = 0n;
		  nLocalMin = 0n;
		  nReset = 0n;
	 }
	 
	 /**
	  *  Interact when Loc min is reached
	  */
	 private def onLocMin(cop : GenericModel){
		  // communicate Local Minimum
		  //solver.communicateLM( this.currentCost, cop.getVariables() as Valuation(sz));
		  val solverState = createSolverState();
		  solver.communicateLM( new State(sz,this.currentCost, cop.getVariables() as Valuation(sz), here.id as Int, solverState) );
	 }
	 
}
public type AdaptiveSearch(s:Long)=AdaptiveSearch{self.sz==s};
