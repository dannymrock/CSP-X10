package csp.solver;
import csp.model.ModelAS;
import x10.util.StringUtil;
import x10.util.RailUtils;
import csp.util.Logger;
import csp.model.ParamManager;
import x10.util.Pair;
import x10.compiler.NonEscaping;
import csp.util.Utils;

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
	 
	 private var tau:Double;
	 private var pdfS:Int;
	 private val selSecond:Int;
	 
	 private var tau1:Double = 1;
	 private var tau2:Double = 0.5;
	 private var tau3:Double = 1;
	 
	 
	 public def this(sizeP:Long, solver:IParallelSolver(sizeP), opts:ParamManager)
	 :EOSearch(sizeP){
		  super(sizeP, solver, opts);
		  this.pdf = new Rail[Double] (sizeP+1, 0.0);// +1 since x in 1..size
		  
		  //fit = new Rail[Pair[Long,Long]](this.sz); 
		  this.fit = new Rail[Long](sizeP, 0);
		  //this.solver = solver;
		  
		  // Parameters
		  this.tau = opts("--EO_tau", (1.0 + 1.0 / Math.log(sz)));
		  this.pdfS = opts("--EO_pdf", 1n);
		  this.selSecond = opts("--EO_selSec", 1n);
		  
		  
		  val t1s = System.getenv("TAU1");
		  if (t1s != null)
				this.tau1 = StringUtil.parseInt(t1s)/ 100.0;
		  
		  val t2s = System.getenv("TAU2");
		  if (t2s != null)
				this.tau2 = StringUtil.parseInt(t2s)/ 100.0;
		  
		  val t3s = System.getenv("TAU3");
		  if (t3s != null)
				this.tau3 = StringUtil.parseInt(t3s)/ 100.0;
		  
		  
		  // if ( this.pdfS == -1n ) // Select a random PDF
		  // {
				// Console.OUT.println("EO: Random PDF ");
		  // }
		  // 
		  // if ( this.tau < 0.0 ) // Select a random tau from 0 to tau 
		  // {
				// Console.OUT.println("EO: Random tau ");
		  // }
		  
		  var PDFname:String = "";
		  if ( this.pdfS == 1n )
				PDFname = "pow";
		  else if(this.pdfS == 2n)
				PDFname = "exp";
		  else if (this.pdfS == 3n)
				PDFname = "gamma";
		  
		  
		  if (here.id == 0)
				Console.OUT.println("Parameters EO: TAU= "+tau+", pdf= "
						  +PDFname+ ", Second_variable_selection="+
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
		  
		  if ( this.pdfS == -1n ) // Select a random PDF
		  {
				this.pdfS = random.nextInt(3n)+1n; // from 1 to 3
		  }
		  
		  if ( this.tau == -1.0 ) // Select a random tau from 0 to tau 
		  {
				if ( this.pdfS == 1n)
					 this.tau = 0.5+random.nextDouble(); // from 0.5 to 1.5
				else if ( this.pdfS == 2n)
					 this.tau = 0.0001+random.nextDouble(); // from 0.0001 to 1.0001
				else if ( this.pdfS == 3n)
					 this.tau = 1.5+random.nextDouble(); // from 1.5 to 2.5
		  }else if ( this.tau == -2.0 ) // different values for each pdf 
		  {
				if ( this.pdfS == 1n)
					 this.tau = this.tau1; // 
				else if ( this.pdfS == 2n)
					 this.tau = this.tau2; // 
				else if ( this.pdfS == 3n)
					 this.tau = this.tau3; // 
		  }
		  
		  // val tStr = System.getenv("T");
		  // val tau = (tStr==null)? (1.0 + 1.0 / Math.log(sz)) : StringUtil.parseLong(tStr)/100.0;
		  // Console.OUT.println(here+"PDF "+this.pdfS+" tau "+this.tau);
		  
		  // val pStr = System.getenv("F");
		  // val pdfS = (pStr==null)? 1n : StringUtil.parseInt(pStr);
		  
		  if ( this.pdfS == 1n )
				initPDF( this.powFnc );
		  else if(this.pdfS == 2n)
				initPDF( this.expFnc );
		  else
				initPDF( this.gammaFnc );
		  
		  Logger.debug(()=>{"EOSolver"});

	 }
	 
	 private def initPDF( fnc:(tau : Double, x : Long)=>Double ){
		  var sum:Double = 0.0;
		  var y:Double = 0.0;
		  
		  for (var x:Int = 1n; x <= this.sz; x++){
				y = fnc(tau, x);
				if (y < 0)
					 y = 0;
				pdf(x) = y;
				sum += y; 
		  }
		  for (var x:Int = 1n; x <= this.sz; x++){
					pdf(x) /= sum;
		  }
		  // for (x in pdf.range())
			//	Console.OUT.println(pdf(x)+" ");//Console.OUT.println( x+"-"+pdf(x)+" ");
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
	 
	 private def selFirstVar( cop_ : ModelAS, move:MovePermutation){
		  var i: Long =-1n;
		  var cost: Long;
		  var selIndex:Long = 0; 
		  var locMin:Boolean = true;
		  
		  while((i = cop_.nextI(i)) as ULong < this.sz as ULong) { //False if i < 0
				cost = cop_.costOnVariable(i);
				// each position on the fit array is divided to contain both i and cost
				// variable index "i" is stored in the 10 LSB 
				// the cost is stored in the remaining MSB 
				fit(i) = cost << 10 | i;
				//Console.OUT.printf("%d %X %X \n",cost,cost,fit(i));
				
				// Detect local min: 
				// can be applied on "first variable selction" only for QAP
				if ( cost > 0 ) // cost is -delta for QAP.
					 locMin = false;
				
		  }
		  RailUtils.sort(fit, cmp);	
		  
		  if (locMin) onLocMin(cop_);
		  
		  // for (v in fit)
		  //   Console.OUT.printf("%d %d \n",(v & 0xFFF),(v >>12));
		  		
		  val index = pdfPick();
		  val sVar = fit(index) & 0x3FF;
		  val sCost = fit(index) >> 10;
		  //Console.OUT.printf("svar %d scost %d \n",sVar,sCost);
		  var nSameFit:Int = 0n;
		 
		  for(var k:Int=0n; k < this.sz; k++){
            val cCost = fit(k) >> 10; 
		      //Console.OUT.printf("cCost %d scost %d \n",cCost,sCost);
				if ( cCost < sCost)   // descending order
					 break;
				
				if (cCost == sCost && random.nextInt(++nSameFit) == 0n)
					 selIndex = fit(k) & 0x3FF;
		  }
		  // Console.OUT.println("index "+index+ " selIndex "+selIndex+ " ");
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
		  
		  // if (minCost > this.currentCost)
		  // 	 this.onLocMin(csp);
		  
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
	  *  Interact when Loc min is reached
	  */
	 private def onLocMin(cop : ModelAS){
		  // communicate Local Minimum
		  solver.communicateLM( this.currentCost, cop.getVariables() as Valuation(sz));
	 }
	 
	 /**
	  *  Interact with other entities
	  */
	 protected def interact( cop_:ModelAS{self.sz==this.sz}){
		  super.interact(cop_);
		  //Change tau
		  // if ( this.pdfS == 1n){
		  // 	 this.tau = 0.5+random.nextDouble(); // from 0.5 to 1.5
		  // 	 initPDF( this.powFnc );
		  // }
		  // else if ( this.pdfS == 2n){
		  // 	 this.tau = 0.0001+random.nextDouble(); // from 0.0001 to 1.0001					 
		  // 	 initPDF( this.expFnc );
		  // }
		  // else if ( this.pdfS == 3n){
		  // 	 this.tau = 1.5+random.nextDouble(); // from 1.5 to 2.5
		  // 	 initPDF( this.gammaFnc );
		  // }
		  // 
	 }	
	 
	 
	 protected def updateCosts(cop : ModelAS){
		  if(this.currentCost < this.bestCost){ //(totalCost <= bestCost)
				Rail.copy(cop.getVariables() as Valuation(sz), this.bestConf as Valuation(sz));
				this.bestCost = this.currentCost;
				
				bestSent = false; // new best found, I must send it!
				
				if (this.reportPart){
					 val eT = (System.nanoTime() - initialTime)/1e9;
					 val gap = (this.bestCost-this.target)/(this.bestCost as Double)*100.0;

					 Utils.show("Solution",this.bestConf);
					 Console.OUT.printf("%s\ttime: %5.1f s\tbest cost: %10d\tgap: %5.2f%% \n",here,eT,this.bestCost,gap);
					 // print on alternative tty
					 //val p = altTty.printer();
					 //p.printf("%s\ttime: %5.1f s\tbest cost: %10d\tgap: %5.2f%% \n",here,eT,this.bestCost,gap);
				}
				
				// Console.OUT.println(here+" best cost= "+bestCost);
				// Compare cost and break if target is accomplished
				if ((this.strictLow && this.bestCost < this.target)
						  ||(!this.strictLow && this.bestCost <= this.target)){
					 Console.OUT.println("Success in "+here+"! tau = "+this.tau+" PDF = "+this.pdfS);
					 this.targetSucc = true;
					 this.kill = true;
				}
		  }
	 }
	 
	 
	 // /**
	 //  *  Update the cost for the optimization variables
	 //  *  Reimplemente here to include communication flag "best send"
	 //  */
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
	 
	 // /**
	 //  * 	Report statistics from the solving process
	 //  */
	 // public def reportStats( c : CSPStats){
	 // super.reportStats(c);
	 // //c.change = this.nChangeV;
	 // }
	 
}
public type EOSearch(s:Long)=EOSearch{self.sz==s};