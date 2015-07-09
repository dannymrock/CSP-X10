package csp.solver;
import csp.model.ModelAS;
import x10.util.StringUtil;
import x10.util.RailUtils;
import csp.util.Logger;

/**
 * Class EOSearch
 */
public class EOSearch extends RandomSearch {
	 	 
	 /** Number time to change vector due to communication */ 
	 private var nbChangeV : Int = 0n;
	 
	 // PDF for EO
	 private val pdf = new Rail[Double](size, 0.0);
	 private val fit = new Rail[PairAS] (size); 
	 //Here i -> index   j->cost (fitness)
	 private val cmp : (PairAS,PairAS) => Int = (a:PairAS,b:PairAS) => {return b.j - a.j ;}; 
	 

	 val powFnc = (tau : Double, x : Int):Double => {
		  return Math.pow(x, -tau);
	 };
	 val expFnc = (tau : Double, x : Int):Double => {
		  return Math.exp(-tau * x);
	 };
	 
	 // Communication Variables
	 private var bestSent:Boolean=false;
	 private solver:IParallelSolver(sz);
	 
	 public def this(sz:Long, size:Int, solver:IParallelSolver(sz), mTime:Long){
		  super(sz, size, mTime);
		  this.solver = solver;
	 }
	 
	 
	  
	 /**
	  *  Search process (in loop functionality)
	  *  To be overwrited for each child class (solver) 
	  */
	 protected def search( cop_ : ModelAS{self.sz==this.sz}) : Int{
		  //Console.OUT.println("EO");
		  
		  selectFirstVar( cop_, move );
		  currentCost = selectVarMinConflict( cop_, move);
		  //newCost = selectSecondVar( cop_ , totalCost, eoi);
		  cop_.swapVariables(move.getFirst(), move.getSecond()); //adSwap(maxI, minJ,csp);
		  nbSwap++;
		  cop_.executedSwap(move.getFirst(), move.getSecond());
		  return currentCost;
	 }
	 
	 protected def initVar( cop_:ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  super.initVar(cop_, tCost, sLow);
		  
		  val tStr = System.getenv("T");
		  val tau = (tStr==null)? (1.0 + 1.0 / Math.log(size)) : StringUtil.parseLong(tStr)/100.0;
		  //Console.OUT.println("tau "+tau);
		  
		  val pStr = System.getenv("F");
		  val pdfS = (pStr==null)? 1n : StringUtil.parseInt(pStr);
		  
		  if ( pdfS == 1n )
				initPDF( tau, size, powFnc );
		  else
				initPDF( tau, size, expFnc );
		  
		  Logger.debug(()=>{"EOSolver"});

	 }
	 
	 private def initPDF(tau:Double, size:Int, fnc:(tau : Double, x : Int)=>Double ){
		  var sum:Double = 0.0;
		  var y:Double = 0.0;
		  
		  for (x in 1n..size){
				y = fnc(tau, x);
				if (y < 0)
					 y = 0;
				pdf(x) = y;
				sum += y; 
		  }
		  for (x in 1n..size){
				pdf(x) /= sum;
		  }
		  // for (x in 1n..size)
		  // Console.OUT.println( x+"-"+pdf(x)+" ");
	 }
	 
	 private def pdfPick():Int {
		  //return pdf(random.nextInt(size)) - 1n;
		  var p:Double = random.nextDouble();
		  var fx:Double;
		  var x:Int = 0n;
		  
		  while((fx = pdf(++x)) < p){
				p -= fx;
		  }
		  return x - 1n ;
	 }
	 
	 private def selectFirstVar( cop_ : ModelAS, move:MovePermutation){
		  var i: Int =-1n;
		  var cost: Int;
		  var selIndex:Int=0n; 
		  
		  while((i = cop_.nextI(i)) as UInt < size as UInt) { //False if i < 0
				cost = cop_.costOnVariable(i);
				fit(i) = new PairAS(i as Int , cost);
		  }
		  //[PairAS]
		  RailUtils.sort(fit, cmp);	
		  val index = pdfPick();
		  val selFit = fit(index).j;
		  var nSameFit:Int = 0n;

		  for(var k:Int=0n; k < size; k++){
				if (fit(k).j < selFit)   // descending order
					 break;
				
				if (fit(k).j == selFit && random.nextInt(++nSameFit)==0n)
					 selIndex = fit(k).i;
		  }
		  //Console.OUT.println("index "+index+ " selIndex "+selIndex+ " ");
		  move.setFirst(selIndex);
	 } 
	 
	 /**
	  * 	selectVarMinConflict( csp : ModelAS) : Int
	  * 	Computes swap and selects the minimum of cost if swap
	  * 	@param csp problem model
	  * 	@return the index of the variable with minimum individual cost if swap
	  */
	 private def selectVarMinConflict( csp : ModelAS, move:MovePermutation) : Int {
		  var j: Int;
		  var cost: Int;
		  var second : Int = 0n;
		  var nSameMin:Int = 0n;
		  var minCost:Int = Int.MAX_VALUE;
		  val first = move.getFirst();
		  
		  //Console.OUT.println("fv = "+ fv+" totalcost "+ totalCost);
		  
		  for (j = 0n; j < size; j++)
		  {	
				if (first == j) continue;
				cost = csp.costIfSwap(this.currentCost, j, first);
				//Console.OUT.println("J = "+ j+" cost "+ cost);
				
				if (cost < minCost){
					 minCost = cost;
					 second = j;
					 nSameMin = 1n;
				} else if (cost == minCost && random.nextInt(++nSameMin) == 0n){
					 second = j;
				}
		  }
		  //Console.OUT.println("minJ = "+ minJ+" newCost "+ minCost+" totalcost "+ totalCost);
		  move.setSecond(second);
		  return minCost;
	 }
	 
	 private def selectSecondVar( csp : ModelAS, totalCost:Int, move:MovePermutation) : Int {
		  val randomJ = random.nextInt(size);
		  val newCost = csp.costIfSwap(totalCost, randomJ, move.getFirst());	 
		  move.setSecond(randomJ);
		  return newCost; 
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
public type EOSearch(s:Long)=EOSearch{self.sz==s};