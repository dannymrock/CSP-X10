package csp.solver;
import csp.model.GenericModel;
import x10.util.StringUtil;
import x10.util.RailUtils;
import csp.util.Logger;
import csp.model.ParamManager;
import x10.util.Pair;
import x10.compiler.NonEscaping;
import csp.util.Utils;
import csp.model.Main;

/**
 * Class EOSearch
 */
public class EOSearch extends RandomSearch {
	 	 
	 /** Number time to change vector due to communication */ 
	 //private var nChangeV : Int = 0n;
	 
	 // PDF for EO
	 private val pdf:Rail[Double];
	 //private val fit:Rail[Pair[Long, Long]];
	 private val fit:Rail[Long];

	 // var index is stored in the 10 LSB and the cost is stored on the remaining MSB
	 private val cmp : (Long,Long) => Int = (a:Long, b:Long) => {
	 			return((b >> 10) - (a >> 10))as Int;
	 			}; 

	 val powFnc = (tau : Double, x : Long):Double => {
		  return Math.pow(x, -tau);
	 };
	 
	 val expFnc = (tau : Double, x : Long):Double => {
		  return Math.exp(-tau * x);
	 };
	 
	 val gammaFnc = (tau : Double, x : Long):Double => {
		  
		  val k = tau;
		  val theta = Math.exp(tau);
		  
		  val constk = Math.pow(theta,k)*EOSearch.gamma(k);
		  
		  val f =  Math.pow(x, k-1) * Math.exp(-x/theta) / constk;
		  
		  return f;
	 };
	
	 static def gamma(n:Double):Double{
		  val invn = 1.0 / n;
		  val g = ( 2.506628274631 * Math.sqrt(invn) + 
					 0.208885689552583 * Math.pow(invn, 1.5) + 
					 0.00870357039802431 * Math.pow(invn, 2.5) - 
					 (174.210665086855 * Math.pow(invn, 3.5)) / 25920.0 - 
					 (715.642372407151 * Math.pow(invn, 4.5)) / 1244160.0
		  ) * Math.exp((-Math.log(invn) - 1) * n);
		  return g;
	 }
	 
	 
	 // Communication Variables
	 //private var bestSent:Boolean = false;
	 //private solver:IParallelSolver(sz);
	 
	 private val tauUserSel:Double;
	 private var tau:Double;
	 private val pdfUserSel:Int; //pdf initial selection
	 private var pdfS:Int = 1n;  //pdf state
	 private val selSecond:Int;
	 
	 private val expDown : Double;
	 private val expUp : Double;
	 private val powDown : Double;
	 private val powUp : Double;
	 
	 
	 
	 public def this(sizeP:Long, solver:IParallelSolver(sizeP), opts:ParamManager)
	 :EOSearch(sizeP){
		  super(sizeP, solver, opts);
		  
		  this.mySolverType = Main.EO_SOL;
		  
		  //Console.OUT.println(here+" EO");
		  this.pdf = new Rail[Double] (sizeP+1, 0.0);// +1 since x in 1..size
		  this.fit = new Rail[Long](sizeP, 0);
		  
		  // Parameters (Random by default)
		  this.tauUserSel = opts("--EO_tau", (1.0 + 1.0 / Math.log(sz)));
		  //this.tauUserSel = opts("--EO_tau", -1.0);
		  this.pdfUserSel = opts("--EO_pdf", -1n);
		  this.selSecond = opts("--EO_selSec", 1n);
		  
		  // Compute interval limit for random tau (based on "force" concept eo-qap implementation Daniel Diaz)
		  this.expDown = 6.385378048 * Math.pow(this.sz,-1.033400799);
		  this.expUp = 8.867754442 * Math.pow(this.sz,-0.895936426);
		  this.powDown = 1.575467001 * Math.pow(this.sz,-0.1448643794);
		  this.powUp = 2.426369897 * Math.pow(this.sz,-0.1435045369);
		  
		  // Show Parameters
		  var PDFname:String = "";
		  if (this.pdfUserSel == -1n)
				PDFname = "Random (exp or pow)";
		  else if(this.pdfUserSel == 2n)
				PDFname = "exp";
		  else if (this.pdfUserSel == 3n)
				PDFname = "gamma";
		  else // ( this.pdfUserSel == 1n ) // 1 and any other number
				PDFname = "pow";
		  if (here.id == 0)
				Console.OUT.println("Parameters EO: TAU=> "+(tauUserSel == -1.0 ? "Random":tauUserSel)+" pdf=> "
						  +PDFname+ " Second_variable_selection=>"+
						  (selSecond==0n?"Random":"MinConflict"));
	 }

	 
	 /**
	  *  Initialize variables of the solver
	  *  Executed once before the main solving loop
	  */
	 protected def initVar( cop_ : GenericModel{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  super.initVar(cop_, tCost, sLow);
		  Logger.debug(()=>{"EOSolver - iniVar"});
		  if ( this.pdfUserSel == -1n ) { // Select a random PDF
				this.pdfS = random.nextInt(2n)+1n; // from 1 to 3
		  }else
				this.pdfS = pdfUserSel;
		  
		  if ( this.tauUserSel == -1.0 ) { // Select a random tau from 0 to tau 
				if ( this.pdfS == 1n) {
					 this.tau = this.powDown + (powUp - powDown) * random.nextDouble();
				}
				else if ( this.pdfS == 2n) {
					 this.tau = this.expDown + (expUp - expDown) * random.nextDouble();
				}
				//Console.OUT.println(here+"pdf "+pdfS+" tau "+this.tau );
		  }
		  else
				this.tau = this.tauUserSel;
		  
		  if (this.pdfS == 3n)
				initPDF( this.gammaFnc );
		  else if (this.pdfS == 2n)
				initPDF( this.expFnc );
		  else //( this.pdfS == 1n )
				initPDF( this.powFnc );
	 }
	 
	 private def initPDF( fnc:(tau : Double, x : Long)=>Double ){
		  var sum:Double = 0.0;
		  var y:Double = 0.0;
		  for (var x:Int = 1n; x <= this.sz; x++){
				y = fnc(this.tau, x);
				pdf(x) = y;
				sum += y; 
		  }
		  for (var x:Int = 1n; x <= this.sz; x++){
				pdf(x) /= sum;
		  }
		  // for (x in pdf.range())
		  //	Console.OUT.println(pdf(x)+" ");//Console.OUT.println( x+"-"+pdf(x)+" ");
	 }

	 /**
	  *  Extremal Search process (in loop functionality)
	  */
	 protected def search( cop_ : GenericModel{self.sz==this.sz}) : Long{
		  //Console.OUT.println("EO");
		  this.selFirstVar( cop_, this.move );
		  if (this.selSecond == 0n)
				currentCost = this.selSecondRandom( cop_, this.move);
		  else 
				currentCost = this.selSecondMinConf( cop_, this.move);
		  	
		  //newCost = selectSecondVar( cop_ , totalCost, eoi);
		  cop_.swapVariables(this.move.getFirst(), this.move.getSecond()); //adSwap(maxI, minJ,csp);
		  nSwap++;
		  cop_.executedSwap(this.move.getFirst(), this.move.getSecond());
		  return currentCost;
	 }
	 
	 
	 private def pdfPick():Int {
		  //return pdf(random.nextInt(this.sz)) - 1n;
		  var p:Double = random.nextDouble();
		  var fx:Double;
		  var x:Int = 0n;
		  while( (fx = pdf(++x)) < p ){
				p -= fx;
		  }
		  return x - 1n ;
	 }
	 
	 private def selFirstVar( cop_ : GenericModel, move:MovePermutation){
		  var i: Long =-1n;
		  var cost: Long;
		  var selIndex:Long = 0; 
		  var locMin:Boolean = true;
		  while((i = cop_.nextI(i)) as ULong < this.sz as ULong) { //False if i < 0
				cost = cop_.costOnVariable(i);
				// each position on the fit array is divided to contain both i and cost
				// variable index "i" is stored in the 10 LSB 
				// the cost is stored in the remaining MSB 
				this.fit(i) = cost << 10 | i;
				//Console.OUT.printf("%d %X %X \n",cost,cost,fit(i));
				
				// Detect local min: 
				// can be applied on "first variable selction" only for QAP
				if ( cost > 0 ) // cost is -delta for QAP.
					 locMin = false;
				
		  }
		  RailUtils.sort(this.fit, this.cmp);	
		  
		  if (locMin) this.onLocMin(cop_);
		  
		  // for (v in fit)
		  //   Console.OUT.printf("%d %d \n",(v & 0xFFF),(v >>12));
		  		
		  val index = this.pdfPick();
		  val sVar = this.fit(index) & 0x3FF;
		  val sCost = this.fit(index) >> 10;
		  //Console.OUT.printf("svar %d scost %d \n",sVar,sCost);
		  var nSameFit:Int = 0n;
		 
		  for(var k:Int=0n; k < this.sz; k++){
            val cCost = this.fit(k) >> 10; 
		      //Console.OUT.printf("cCost %d scost %d \n",cCost,sCost);
				if ( cCost < sCost)   // descending order
					 break;
				
				if (cCost == sCost && random.nextInt(++nSameFit) == 0n)
					 selIndex = fit(k) & 0x3FF;
		  }
		  // Console.OUT.println("index "+index+ " selIndex "+selIndex+ " ");

		  // Save first variable selected into the move object
		  this.move.setFirst(selIndex);
	 } 
	 
	 /**
	  * 	selectVarMinConflict( csp : ModelAS) : Int
	  * 	Computes swap and selects the minimum of cost if swap
	  * 	@param csp problem model
	  *   @param move object
	  * 	@return the cost of the best move
	  */
	 private def selSecondMinConf( csp : GenericModel, move:MovePermutation) : Long {
		  var j: Long;
		  var cost: Long;
		  var second : Long = 0;
		  var nSameMin:Int = 0n;
		  var minCost:Long = Long.MAX_VALUE;
		  val first = this.move.getFirst();
		  
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
		  
		  // if (minCost > this.currentCost)
		  // 	 this.onLocMin(csp);
		  
		  //Console.OUT.println("minJ = "+ minJ+" newCost "+ minCost+" totalcost "+ totalCost);
		  // Save second variable selected into the move object
		  this.move.setSecond(second);
		  return minCost;
	 }
	 
	 
	 
	 private def selSecondRandom( cop : GenericModel, move:MovePermutation) : Long {
		  val randomJ = random.nextLong(this.sz);
		  val newCost = cop.costIfSwap(this.currentCost, randomJ, this.move.getFirst());	 
		  this.move.setSecond(randomJ);
		  return newCost; 
	 }
	 
	 /**
	  *  Interact when Loc min is reached
	  */
	 private def onLocMin(cop : GenericModel){
		  // communicate Local Minimum
		  // solver.communicateLM( this.currentCost, cop.getVariables() as Valuation(sz));
		  val solverState = this.createSolverState();
		  this.solver.communicateLM( new State(sz, this.currentCost, cop.getVariables() as Valuation(sz), here.id as Int, solverState) );
	 }
	
	 
	 /**
	  *  Create EO Solver State array to be send to Pool
	  *  oeState(0) = solverType  
	  *  oeState(1) = EO pdf type
	  *  oeState(2) = EO "tau" value
	  */
	 protected def createSolverState( ) : Rail[Int]{self.size==3}{
		  val eoState = new Rail[Int](3,-1n);
		  eoState(0) = this.mySolverType;
		  eoState(1) = this.pdfS;
		  eoState(2) = (this.tau * 1000.0) as Int; // TODO: convert double to Int??? levels ranges ???
		  return eoState;  
	 }
	 
	 /**
	  *  Process Solver State Array received from Pool
	  * 
	  */
	 protected def processSolverState( state : Rail[Int]{self.size==3}){
		  // Random Search has no parameters to process
		  
		  val inSolverType = state(0);
		   
		  if (inSolverType == this.mySolverType){
				val inpdf = state(1);
				val intau = state(2) / 1000.0;
				if (this.pdfS == inpdf) {
					 //Console.OUT.println(here+" Changing Tau");
					 //this.tau = (intau + this.tau) / 2.0;
					 this.tau = intau; 
				} else {
					 if ( this.pdfS == 1n) {
						  this.tau = this.powDown + (powUp - powDown) * random.nextDouble();
					 }
					 else if ( this.pdfS == 2n) {
						  this.tau = this.expDown + (expUp - expDown) * random.nextDouble();
					 }
				}
				
				if (this.pdfS == 3n)
					 initPDF( this.gammaFnc );
				else if (this.pdfS == 2n)
					 initPDF( this.expFnc );
				else //( this.pdfS == 1n )
					 initPDF( this.powFnc );
		  }
	 } 	 
}
public type EOSearch(s:Long)=EOSearch{self.sz==s};