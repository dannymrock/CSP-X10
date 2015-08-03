package csp.solver;
import csp.model.ModelAS;
import x10.util.StringUtil;
import x10.util.RailUtils;
import csp.util.Logger;
import csp.model.ParamManager;
import x10.util.Pair;

/**
 * Class EOSearch
 */
public class EOSearch extends RandomSearch {
	 	 
	 /** Number time to change vector due to communication */ 
	 private var nChangeV : Int = 0n;
	 
	 // PDF for EO
	 private val pdf:Rail[Double];
	 private val fit:Rail[Pair[Long, Long]];
	 //Here i -> index   j->cost (fitness)
	 private val cmp : (Pair[Long,Long],Pair[Long,Long]) => Int = 
		  (a:Pair[Long,Long], b:Pair[Long, Long]) => {return b.second as Int - a.second as Int ;}; 
	 

	 val powFnc = (tau : Double, x : Long):Double => {
		  return Math.pow(x, -tau);
	 };
	 val expFnc = (tau : Double, x : Long):Double => {
		  return Math.exp(-tau * x);
	 };
	 
	 // Communication Variables
	 private var bestSent:Boolean = false;
	 private solver:IParallelSolver(sz);
	 private val tau:Double;
	 private val pdfS:Int;
	 private val selSecond:Int;
	 
	 public def this(sz:Long, solver:IParallelSolver(sz), opts:ParamManager){
		  super(sz, opts);
		  this.pdf = new Rail[Double] (this.sz, 0.0);
		  fit = new Rail[Pair[Long,Long]](this.sz); 
		  this.solver = solver;
		  
		  // Parameters
		  this.tau = opts("--EO_tau", (1.0 + 1.0 / Math.log(sz)));
		  this.pdfS = opts("--EO_pdf", 1n);
		  
		  this.selSecond = opts("--EO_selSec", 1n);
		  
		  if (here.id == 0)
				Console.OUT.println("Parameters EO: TAU= "+tau+", pdf= "
						  +(pdfS == 1n ? "Power":"Exp")+ ", Second_variable_selection="+
						  (selSecond==0n?"Random":"MinConflict"));

	 }
	 
	 
	  
	 /**
	  *  Search process (in loop functionality)
	  *  To be overwrited for each child class (solver) 
	  */
	 protected def search( cop_ : ModelAS{self.sz==this.sz}) : Long{
		  //Console.OUT.println("EO");
		  
		  selFirstVar( cop_, move );
		  if (selSecond == 0n)
				currentCost = selSecondRandom( cop_, move);
		  else 
				currentCost = selSecondMinConf( cop_, move);
		  	
		  //newCost = selectSecondVar( cop_ , totalCost, eoi);
		  cop_.swapVariables(move.getFirst(), move.getSecond()); //adSwap(maxI, minJ,csp);
		  nSwap++;
		  cop_.executedSwap(move.getFirst(), move.getSecond());
		  return currentCost;
	 }
	 
	 protected def initVar( cop_:ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  super.initVar(cop_, tCost, sLow);
		  
		  // val tStr = System.getenv("T");
		  // val tau = (tStr==null)? (1.0 + 1.0 / Math.log(sz)) : StringUtil.parseLong(tStr)/100.0;
		  //Console.OUT.println("tau "+tau);
		  
		  // val pStr = System.getenv("F");
		  // val pdfS = (pStr==null)? 1n : StringUtil.parseInt(pStr);
		  
		  if ( this.pdfS == 1n )
				initPDF( this.powFnc );
		  else
				initPDF( this.expFnc );
		  
		  Logger.debug(()=>{"EOSolver"});

	 }
	 
	 private def initPDF( fnc:(tau : Double, x : Long)=>Double ){
		  var sum:Double = 0.0;
		  var y:Double = 0.0;
		  
		  for (x in 1n..this.sz){
				y = fnc(tau, x);
				if (y < 0)
					 y = 0;
				pdf(x) = y;
				sum += y; 
		  }
		  for (x in 1n..this.sz){
				pdf(x) /= sum;
		  }
		  // for (x in 1n..this.sz)
		  // Console.OUT.println( x+"-"+pdf(x)+" ");
	 }
	 
	 private def pdfPick():Int {
		  //return pdf(random.nextInt(this.sz)) - 1n;
		  var p:Double = random.nextDouble();
		  var fx:Double;
		  var x:Int = 0n;
		  
		  while((fx = pdf(++x)) < p){
				p -= fx;
		  }
		  return x - 1n ;
	 }
	 
	 private def selFirstVar( cop_ : ModelAS, move:MovePermutation){
		  var i: Long =-1n;
		  var cost: Long;
		  var selIndex:Long = 0; 
		  
		  while((i = cop_.nextI(i)) as ULong < this.sz as ULong) { //False if i < 0
				cost = cop_.costOnVariable(i);
				fit(i) = new Pair(i , cost );
		  }
		  //[PairAS]
		  RailUtils.sort(fit, cmp);	
		  val index = pdfPick();
		  val selFit = fit(index).second;
		  var nSameFit:Int = 0n;

		  for(var k:Int=0n; k < this.sz; k++){
				if (fit(k).second < selFit)   // descending order
					 break;
				
				if (fit(k).second == selFit && random.nextInt(++nSameFit)==0n)
					 selIndex = fit(k).first;
		  }
		  //Console.OUT.println("index "+index+ " selIndex "+selIndex+ " ");
		  move.setFirst(selIndex);
	 } 
	 
	 /**
	  * 	selectVarMinConflict( csp : ModelAS) : Int
	  * 	Computes swap and selects the minimum of cost if swap
	  * 	@param csp problem model
	  *   @param move object
	  * 	@return the cost of the best move
	  */
	 private def selSecondMinConf( csp : ModelAS, move:MovePermutation) : Long {
		  var j: Long;
		  var cost: Long;
		  var second : Long = 0;
		  var nSameMin:Int = 0n;
		  var minCost:Long = Long.MAX_VALUE;
		  val first = move.getFirst();
		  
		  //Console.OUT.println("fv = "+ fv+" totalcost "+ totalCost);
		  
		  for (j = 0; j < this.sz; j++)
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
	 
	 private def selSecondRandom( csp : ModelAS, move:MovePermutation) : Long {
		  val randomJ = random.nextLong(this.sz);
		  val newCost = csp.costIfSwap(this.currentCost, randomJ, move.getFirst());	 
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
	 c.change = this.nChangeV;
	 }
	 
}
public type EOSearch(s:Long)=EOSearch{self.sz==s};