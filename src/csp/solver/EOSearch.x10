package csp.solver;
import csp.model.ModelAS;
import x10.util.StringUtil;
import x10.util.RailUtils;
import csp.util.Logger;
import csp.model.ParamManager;

/**
 * Class EOSearch
 */
public class EOSearch extends RandomSearch {
	 	 
	 /** Number time to change vector due to communication */ 
	 private var nbChangeV : Int = 0n;
	 
	 // PDF for EO
	 private val pdf:Rail[Double];
	 private val fit:Rail[PairAS];
	 //Here i -> index   j->cost (fitness)
	 private val cmp : (PairAS,PairAS) => Int = (a:PairAS,b:PairAS) => {return b.j - a.j ;}; 
	 

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
	 
	 public def this(sz:Long, solver:IParallelSolver(sz), opts:ParamManager){
		  super(sz, opts);
		  this.pdf = new Rail[Double] (this.sz, 0.0);
		  fit = new Rail[PairAS] (this.sz); 
		  this.solver = solver;
		  
		  // Parameters
		  this.tau = opts("--EO_tau", (1.0 + 1.0 / Math.log(sz)));
		  this.pdfS = opts("--EO_pdf", 1n);
		  
		  if (here.id == 0)
				Console.OUT.println("Parameters EO: TAU= "+tau+" pdf= "
						  +(pdfS == 1n ? "Power":"Exp"));

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
	 
	 private def selectFirstVar( cop_ : ModelAS, move:MovePermutation){
		  var i: Long =-1n;
		  var cost: Int;
		  var selIndex:Int=0n; 
		  
		  while((i = cop_.nextI(i)) as ULong < this.sz as ULong) { //False if i < 0
				cost = cop_.costOnVariable(i);
				fit(i) = new PairAS(i as Int , cost);
		  }
		  //[PairAS]
		  RailUtils.sort(fit, cmp);	
		  val index = pdfPick();
		  val selFit = fit(index).j;
		  var nSameFit:Int = 0n;

		  for(var k:Int=0n; k < this.sz; k++){
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
	  *   @param move object
	  * 	@return the cost of the best move
	  */
	 private def selectVarMinConflict( csp : ModelAS, move:MovePermutation) : Int {
		  var j: Long;
		  var cost: Int;
		  var second : Long = 0;
		  var nSameMin:Int = 0n;
		  var minCost:Int = Int.MAX_VALUE;
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
	 
	 private def selectSecondVar( csp : ModelAS, totalCost:Int, move:MovePermutation) : Int {
		  val randomJ = random.nextLong(this.sz);
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
				
				if (this.reportPart){
					 val eT = (System.nanoTime() - initialTime)/10e9;
					 val gap = (this.bestCost-this.target)/(this.bestCost as Double)*100.0;
					 Console.OUT.printf("%s\ttime: %5.1f s\tbest cost: %10d\tgap %5.2f%% \n",here,eT,this.bestCost,gap);
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
	 
}
public type EOSearch(s:Long)=EOSearch{self.sz==s};