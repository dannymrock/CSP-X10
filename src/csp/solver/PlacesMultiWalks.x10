/** PlaceMultiWalk is the parallel implementation of Random Walk Adaptive Search solver
 *  in the X10 language. This implementation use distributed isolated instances
 *  of the solver, each one with a diferent seeds in order to have differents 
 *  scanning walks in the search space.
 * 
 *  This implementation distribute the solver instances across places.
 * 
 *  @author Danny Munera
 *  @version 0.1    9 April, 2013  -> First Version
 *                  10 April, 2013 -> Changes queens by costas problem
 *                  12 April, 2013 -> TLP support
 */

package csp.solver;
import x10.util.Random;
import x10.array.*;
import x10.compiler.Inline;
import x10.util.concurrent.AtomicBoolean; 
import x10.util.Team;
import x10.util.StringUtil;
import x10.io.File;
import x10.io.Printer;
import x10.util.RailUtils;
import csp.solver.CSPSharedUnit;
import csp.model.ParamManager;
import csp.util.Logger;
import csp.util.Utils;
import csp.model.ModelAS;

/**
 * Each place has solvers, a PlaceLocalHandle[PlaceMultiWalk(sz)].
 * The standard way for code at place p to see the state at place q is
 * to execute 
 * <verbatim>
 * at(q) async { 
 *    val thisSolver = solvers(); 
 *    ... now thisSolver.csp gets you the local model,
 *    ...  thisSolver.solver gets you the local solver,
 *    ... etc
 *  >
 * </verbatim>
 */
public class PlacesMultiWalks(sz:Long) implements IParallelSolver {  
	 property sz()=sz;
	 // Shared state, accessible from any place, via at(
	 var csp_:ModelAS(sz);
	 var solver:RandomSearch(sz);
	 
	 var time:Long;	
	 
	 var bcost : Long;
	 val stats = new CSPStats();
	 val sampleAccStats = new CSPStats();
	 val genAccStats = new CSPStats();
	 
	 
	 /** Comunication Variables */
	 var commM : CommManager(sz);
	 //Hybrid approach
	 val expPerTeam : Int;
	 val nTeams : Int;
	 
	 val bestC = new Rail[Int](sz,0n); 
	 
	 var seed:Long;
	 
	 
	 //InterTeam Communication
	 var interTeamKill:Boolean = false;
	 val outTeamTime:Long;
	 val minDistance:Double;
	 
	 val verify:Boolean;
	 
	 var cGroupReset:Int = 0n;
	 
	 val iniDelay:Long;
	 
	 val affectedPer : Double;
	 
	 var avgDis:Double = 0.0;
	 var cDis:Int = 0n;
	 var minDis:Double = 1.0;
	 var maxDis:Double = 0.0;
	 
	 val bestSolHere : Rail[Int];
	 
	 var solString : String =  new String();
	 
	 val compAVG:Int;
	 val opts:ParamManager;
	 
	 val confArray:Rail[CSPSharedUnit];
	 
	 val debug:Boolean;
	 val altTty:File;
	 val p:Printer;
	 
	 
	 /**
	  * 	Constructor of the class
	  */
	 public def this(vectorSize:Long, opt:ParamManager){
		  property(vectorSize);
		  this.opts = opt;
		  // Parameters of the CPLS framework
		  this.verify = opts("-v", 0n) == 1n; // verify == 1 : true
		  this.expPerTeam = opts("-N", 1n);
		  // Inter team communication
		  this.outTeamTime = opts("-I", 0);
		  this.minDistance = opts("-D", 0.3);
		  this.iniDelay = opts("-W", 0);
		  this.affectedPer = opts("-A", 1.0);
		  
		  // Compute the average final cost on each place (tune parameters in seq)
		  this.compAVG = opts("-ca", 0n);
		  
		  //commOption = commOpt;
		  this.nTeams = Place.MAX_PLACES as Int / expPerTeam ;
		  this.bestSolHere = new Rail[Int](vectorSize, 0n);	 
		  
		  this.confArray = new Rail[CSPSharedUnit](nTeams, CSPSharedUnit(sz,-1n,null,-1n,-1.0,-1n)); 
		  
		  val ttyName = opts("-dbg", "none");
		  if (ttyName.equals("none")){
				this.debug = false;
				this.altTty = null;
				this.p = null;
		  }else{
				this.debug = true;
				this.altTty = new File(ttyName);
				this.p = altTty.printer();
		  }
	 }
	 
	 
	 public def installSolver(st:PlaceLocalHandle[IParallelSolver(sz)], solGen:()=>RandomSearch(sz) ):void{
		  solver = solGen();
		  commM = new CommManager(sz, opts, st, nTeams); 
	 }
	 
	 //var option : Long = 0;
	 /** 
	  * 	Solve the csp problem with MAX_PLACES instance of AS solver
	  * 	The first one that reach a valid solution sends a kill to the others
	  * 	to finish the process.
	  * 
	  * 	@param size size of the csp problem
	  * 	@param cspProblem code with the problem to be solved (1 for Magic Square Problems, other number for Queens Problem)
	  * 	@return cost of the solution
	  */
	 var tcost:Int;
	 public def solve(st:PlaceLocalHandle[IParallelSolver(sz)], cspGen:()=>ModelAS(sz), seed_ :Long, targetCost : Long, strictLow: Boolean ):void
	 { 
		  tcost = targetCost as Int;
		  stats.setTarget(tcost);
		  sampleAccStats.setTarget(tcost);
		  genAccStats.setTarget(tcost);
		  
		  val solvers = st;
		  assert solvers() == this : "Whoa, basic plumbing problem -- I am not part of solvers!";
		  
		  this.seed = seed_;
		  val random = new Random(seed);
		  //val random = new Random(here.id);
		  
		  var cost:Long = Long.MAX_VALUE;
		  
		  commM.setSeed(random.nextLong());
		  solver.setSeed(random.nextLong()); 
		  
		 // val optStr = System.getenv("O");
		 // option = (optStr==null)? 0 : StringUtil.parseLong( optStr );
		  
		  //Logger.info(()=>{"   Seed in solver:"+seed});
		  //Console.OUT.println("   Seed in solver:"+seed);
		  
		  interTeamKill = false;
		  
		  // verify if inter team comm is able, if the number of teams is greater than 1 and 
		  //        if place(here) is a head node 
		  //if (outTeamTime > 0 && nTeams > 1n && here.id < nTeams) //node O to nteams
		  if (outTeamTime > 0 && nTeams > 1n && here.id == commM.LOCAL_MIN_NODE)
				// if (outTeamTime > 0 && nTeams > 1n && here.id >= nTeams && here.id < nTeams+nTeams) 
		  {
				//val delay = random.nextLong(outTeamTime);
				//Logger.debug(()=>{" creating Inter-Team Activity"});
				//val delayStr = System.getenv("D");
				//Console.OUT.println(here+"iniDelay "+iniDelay);
				//val delay:Int = (delayStr==null)? 0n : StringUtil.parseInt(delayStr);
				async
				{
					 System.sleep(iniDelay);
					 interTeamActivity(st, random.nextLong());
				} 
		  }
		  
		  
		  csp_ = cspGen(); // use the supplied generator to generate the problem
		  csp_.setSeed(random.nextLong()); //This is important to ensure different paths into each problem
		  
		  //Logger.info(()=>"  PlacesMultiWalks: Start solve process: solver.solve() function ");
		  
		  time = -System.nanoTime();
		  cost = solver.solve(csp_, targetCost, strictLow);
		  time += System.nanoTime();
		  
		  // Logger.debug(()=>"  PlacesMultiWalks: end solve process: solver.solve() function ");
		  //if (cost == 0n){ //TODO: Define a new condition (It's possible to finish without cost=0)
		  
		  
		  interTeamKill = true;
		  //Runtime.probe();
		  
		  if ( ( strictLow && cost < targetCost ) || (!strictLow && cost <= targetCost) )
		  {
				// A solution has been found! Huzzah! 
				// Light the candles! Kill the blighters!
				val home = here.id;
				val winner = at(Place.FIRST_PLACE) solvers().announceWinner( solvers, home );
				
				//winPlace = here;
				bcost = cost;
				
				if (winner) 
				{ 
					 
					 setStats_(solvers);
					 if (verify)
					 {
					   csp_.displaySolution(solver.getBestConfiguration());
					   Console.OUT.println(", Solution is " + 
					 		 (csp_.verify(solver.getBestConfiguration())? "perfect !!!" : "not perfect "));
					   //Console.OUT.println("," + csp_.verify(solver.getBestConfiguration()));
					 } 
					 // else Console.OUT.println();
				}
		  } else
		  {
				solString = "Solution "+here+ " is "+(csp_.verify(solver.getBestConfiguration())? "perfect !!!" : "not perfect, maybe wrong ...");
				Rail.copy(solver.getBestConfiguration(),bestSolHere as Valuation(sz));
		  }			
		  
		  // if (verify){
				// csp_.displaySolution(solver.getBestConfiguration());
				// Console.OUT.println("   Solution is " + 
				// 		  (csp_.verify(solver.getBestConfiguration())? "ok" : "WRONG"));
		  // }
		  
		  
		  //Distance Statistics
		  // if(here.id < nTeams){
		  // 	Console.OUT.printf("\nteam %d avgDis:%4.3f minDis:%4.3f max Dis:%4.3f \n",here.id,(avgDis/cDis),minDis,maxDis);
		  // 	//Console.OUT.println("team "+here.id+" avg dis "+(avgDis/cDis)+" minDis "+minDis+" max Dis "+maxDis);
		  // }
		  
	 }
	 
	 //@Inline 
	 public def getIPVector(csp_:ModelAS(sz), myCost:Long):Boolean 
	 = commM.getIPVector(csp_, myCost);
	 
	 //@Inline 
	 public def getLM(): Maybe[CSPSharedUnit(sz)] 
	 = commM.getLM();
	 
	 //@Inline 
	 public def getPR(): Maybe[CSPSharedUnit(sz)]
	 = commM.getPR();
	
	 public def communicate( info : CSPSharedUnit(sz) )
	 {
		  commM.communicate( info );
	 }
	 
	 @Inline public def inTeamReportI():Int = commM.inTeamReportI;
	 @Inline public def inTeamUpdateI():Int = commM.inTeamUpdateI;
	 
	 public def communicateLM( info:CSPSharedUnit(sz) )
	 {
		  // If diversification mechanism is activated, then send info 
		  if (outTeamTime > 0 && nTeams > 1n ){
				commM.communicateLM( info );
		  }
	 }
	 	 
	 public def kill()
	 {
		  if (solver != null) 
		  {
				solver.kill(); //solver.kill.set(true); //
				interTeamKill = true;
				//Logger.debug(()=>{"Kill=true"});
		  }else
		  {
				Logger.debug(()=>{"Solver is not yet started. Kill is not set"});	
		  }
	 }
	 
	 val winnerLatch = new AtomicBoolean(false);
	 
	 public def announceWinner(ss:PlaceLocalHandle[IParallelSolver(sz)], p:Long):Boolean 
	 {
		  //Logger.debug(()=> "  PlacesMultiWalks: announceWinner " );
		  val result = winnerLatch.compareAndSet(false, true);
		  
		  //Logger.debug(()=> "  PlacesMultiWalks: announceWinner result=" + result + " for " + p + " this=" + this );
		  if (result) 
		  {
				for (k in Place.places()) 
					 if (p != k.id) 
						  at(k) ss().kill(); // at(k) async ss().kill();  // Testing the use of this async v1
		  }
		  //Logger.debug(()=> "  PlacesMultiWalks: announceWinner all kill messages are sent" );
		  
		  return result;
	 }
	 
	 /**
	  * Called by verifyWinner to print the verification info for the best place
	  */
	 public def verify_(ss:PlaceLocalHandle[IParallelSolver(sz)])
	 {
		 
		  Utils.show("Solution",bestSolHere);
		  Console.OUT.println(solString);
		  
	 }
	 
	 /**
	  * Called by winning place to set the stats at place zero so they
	  * can be printed out.
	  */
	 public def setStats_(ss:PlaceLocalHandle[IParallelSolver(sz)])
	 {
		  val winPlace = here.id;
		  val time = time/1e9;
		  // val iters = solver.nbIterTot;
		  // val locmin = solver.nbLocalMinTot;
		  // val swaps = solver.nbSwapTot;
		  // val reset = solver.nbResetTot;
		  // val same = solver.nbSameVarTot;
		  // val restart = solver.nbRestart;
		  // val change = solver.nbChangeV;
		  // val singles = solver.bestCost % sz;
		  // val bp = (solver.bestCost-singles)/sz;
		  // val fr = solver.nbForceRestart;
		  // val target = solver.targetSucc;
		  // val cost = solver.bestCost;
		  val c = new CSPStats();
		  solver.reportStats(c);
		  
		  val head = here.id % nTeams;
		  val gR = at(Place(head)) ss().getGroupReset();
		  //Console.OUT.println("\n\nGroup "+head+" Reset "+gReset);
		  
		  val gReset = (c.forceRestart > gR)? c.forceRestart : gR;
		  
		  val fft = c.cost - tcost;
		  c.time = time;
		  c.team = winPlace as Int;
		  c.groupR = gReset;
		  c.fftarget = fft as Int;
		  c.explorer = 0n; //To notify that there was a solution
		  
		  at (Place.FIRST_PLACE) /*async*/ 
		      ss().setStats(c);
		  //ss().setStats(cost, winPlace as Int, 0n, time, iters, locmin, swaps, reset, same, restart, change,fr, 
					 //bp as Int, singles as Int, gReset, target, fft);
	 }
	 
	 public def setStats(co : Int, p : Int, e : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int, 
				fr : Int, gr:Int, tar:Boolean, fftar:Int)
	 {
		  stats.setStats(co, p, e, t, it, loc, sw, re, sa, rs, ch, fr, gr, tar, fftar, this.sz);
		  accStats(stats);
	 }
	 
	 public def setStats(c:CSPStats)
	 {
		  stats.setStats(c);
		  accStats(stats);
	 }
	 
	 public def printStats(count:Int, oF:Int, problem:Int):void
	 {
		  stats.print(count,oF,problem);
	 }
	 
	 public def printAVG(count:Int, oF:Int, problem:Int):void
	 {
		  sampleAccStats.printAVG(count,oF,problem);
	 }
	 
	 
	 public def printGenAVG(count:Int, oF:Int, problem:Int):void 
	 {
		  genAccStats.printAVG(count,oF, problem);
	 }
	 
	 public def tryInsertConf( info : CSPSharedUnit(sz) ) 
	 {
		  commM.ep.tryInsertConf(  info );
	 }
	 
	 public def tryInsertLM( info : CSPSharedUnit(sz) ) 
	 {
		  commM.lmp.tryInsertConf( info ); 
	 }
	 
	 public def getConf():Maybe[CSPSharedUnit(sz)] = commM.ep.getPConf();
	 public def getLMConf():Maybe[CSPSharedUnit(sz)] = commM.lmp.getPConf();
	 
	 
	 public def getBestConf():Maybe[CSPSharedUnit(sz)] = commM.ep.getBestConf();
	 
	 public def clear()
	 {
		  winnerLatch.set(false);
		  commM.ep.clear();
		  commM.lmp.clear();
		  stats.clear();
		  bestC.clear();
		  solver.clear();
		  //interTeamKill = false;
		  cGroupReset = 0n;
		  //Console.OUT.println(here+" clear");
	 }
	 
	 public def clearSample()
	 {
		  sampleAccStats.clear();
	 }
	 
	 public def accStats(c:CSPStats):void 
	 {
		  genAccStats.accStats(c);
		  sampleAccStats.accStats(c);
	 }
	 
	 // public def getCurrentData():Maybe[CSPSharedUnit(sz)]{
	 // 	var sol:CSPSharedUnit(sz) = new CSPSharedUnit(sz,solver.totalCost,csp_.variables, 
	 // 			here.id as Int);
	 // 	return new Maybe(sol);
	 // }
	 
	 public def getCost():Long {
		  return solver.getBestCost();
	 }
	 
	 public def verifyWinner(ss:PlaceLocalHandle[IParallelSolver(sz)]):void
	 {
		  // detect if no winner has been found
		  // search best solution in all places
		  // set stats objects
		  var minCost:Long = Long.MAX_VALUE;
		  var bestPlace:Place = here; 
		  
		  if (this.compAVG == 1n){
				//Compute avg cost in places
				var avgCost:Long = 0;
				for (k in Place.places())
				{
					 //val cBP = at(k) ss().getBP();
					 val cCost = at(k) ss().getCost();
					 Console.OUT.println(k+"\t"+ cCost);
					 avgCost += cCost;
				}
				Console.OUT.println("avgCost\t"+(avgCost/Place.MAX_PLACES));
				// end avg cost
		  }
		  
		  if (stats.explorer == -1n)
		  {
				Logger.debug(()=>"No winner found");
				
				for (k in Place.places())
				{
					 //val cBP = at(k) ss().getBP();
					 val cCost = at(k) ss().getCost();
					 
					 if(cCost < minCost)
					 {
						  minCost = cCost;
						  bestPlace = k;
					 }
				}
				//val s = minCost % sz; val bp = (minCost-s)/sz;
				//Console.OUT.println("Cost = "+minCost+" Singles= "+s+ "BP= "+bp);
				val place = bestPlace; val mC = minCost;
				Logger.debug(()=>"winner "+ place + " final cost "+ mC);
				
				val ver = this.verify;
				at (bestPlace)
				{
					 ss().setStats_(ss);
					 if (ver)
					 {
						  ss().verify_(ss);
					 }
				}
		  }
	 }
	 	 
	 //var err:Int=0n;
	 /**
	  * Inter Team Communication Functions
	  * */
	 public def interTeamActivity(st:PlaceLocalHandle[IParallelSolver(sz)], seed:Long){
		  val r = new Random(seed);
		  while (!interTeamKill) {
				Logger.debug(()=>{" kill "+interTeamKill});
				
				if (!System.sleep(outTeamTime)){ 
					 //Logger.info(()=>"interTeamActivity error: cannot execute sleep");
					 Console.OUT.println(here+" interTeamActivity error: cannot execute sleep");
					 //err++;
					 continue;
				}
				//while(commM.ep.countInsert % 10n != 0n);
				
				// woken up
				Logger.info(()=>{" interTeamActivity - run : woken up (every "+outTeamTime+" ms)"});
				//val random = new Random(seed);
				//if (random.nextInt(100n) < 16) 
				interTeamComm(st, r);
				Runtime.probe();		// Give a chance to the other activities
		  }
	 }
	 
	 
	 private val cmp : (CSPSharedUnit,CSPSharedUnit) => Int = (a:CSPSharedUnit, b:CSPSharedUnit) => {
		  return(a.cost - b.cost) as Int;
	 };
	 
	 public def interTeamComm(ss:PlaceLocalHandle[IParallelSolver(sz)], r:Random){
		  var teamToRest:Long = -1;
		  
		  //for ( head in 0..(nTeams-1) ) 
		  for ( var head:Int = 0n; head < nTeams; head++) {
				val h = head;
				val conf = at( Place(h) ) ss().getBestConf();
				if (conf == null) {
					 confArray(h) = CSPSharedUnit(sz, -1, null, h as Int,-1.0,-1n);
			   } else {
			   	 confArray(h) = CSPSharedUnit(sz, conf().cost, conf().vector, h as Int, conf().tau, conf().pdf);
			   }
		  }
		  
		  var nEqTeams:Int = 0n;
		  //var eqTeam:Long = -1;
		  
		  val eqTeams : Rail[Long] =  new Rail[Long](nTeams, -1);
		  
		  RailUtils.sort(confArray, cmp);
		  var c:Int; 
		  // Console.OUT.println("0 cost "+confArray(0).cost+" team "+confArray(0).place+" fvalue "+
				// 	 ((confArray(0).cost == -1)?"null":confArray(0).vector(0)));
		  for (c = 0n; c < nTeams - 1 ; c++) {
				// Console.OUT.print((c+1) +" cost "+confArray(c+1).cost+" team "+confArray(c+1).place+" fvalue "+
				// 		  ((confArray(c+1).cost == -1)?"null":confArray(c+1).vector(0)));
				// if (confArray(c).cost != -1)
				// 	 Console.OUT.println(" distance "+csp_.distance( confArray(c).vector as Valuation(sz),
				// 				confArray(c+1).vector as Valuation(sz)));
				
		  
				if (confArray(c).cost != -1 && confArray(c).cost == confArray(c + 1).cost 
						  && csp_.distance( confArray(c).vector as Valuation(sz),
									 confArray(c+1).vector as Valuation(sz)) == 0.0){
					 // Team c is equal to c+1
					 eqTeams( nEqTeams++ ) = confArray(c+1).place;
				} else if ( nEqTeams > 0n && confArray(c).cost != confArray(c + 1).cost )
					 break;
		  }
		  
		  var worstTeam:Long = confArray(nTeams - 1).place; 
		  
		  
		  if (nEqTeams == 0n && worstTeam == -1){
				if (debug) {
					 p.print("\033[H\033["+(nTeams+2)+"B");
					 p.printf("\033[2K\rRestart Team   N/A");
					 p.flush();
				}
				return;	
		  }
		  
		  if (nEqTeams == 0n && worstTeam != -1)
				eqTeams( nEqTeams++ ) = worstTeam;
		  
		  if (debug) {
				p.print("\033[H\033["+(nTeams+2)+"B");
				p.printf("\033[2K\rRestart Team ");
				for (var rp:Long = 0; rp < nEqTeams; rp++) {
					 p.printf(" %3d",eqTeams(rp));				
				}
				p.flush();
		  }
		  
		  // Console.OUT.print("Restart Teams:");
		  for (var rp:Long = 0; rp < nEqTeams; rp++) {
				teamToRest = eqTeams(rp);
				val ttr = teamToRest; 
				
				//Console.OUT.print(" "+teamToRest);
				

				// Count total group partial restart
				at( Place(teamToRest) ) ss().incGroupReset(); 
				Logger.info(()=>{"reset team "+ttr});
				
				for (var i:Long = teamToRest; i < Place.MAX_PLACES; i += nTeams){
					 val vali = i;
					 Logger.info(()=>{"MW - interTeamComm : send signal force Restart on place "+vali});
					 if (r.nextDouble() <= affectedPer)
						  at(Place(i)) ss().diversify();
				}
				at(Place(teamToRest)) ss().clearIntPool();
		  }
		  //Console.OUT.println(" ");
		  
	 }
	 
	 public def getGroupReset():Int{
		  return this.cGroupReset;
	 }
	 
	 public def incGroupReset():void{
		  this.cGroupReset++;
	 }
	 
	 public def clearIntPool():void{
		  commM.ep.clear();		
	 }
	 
	 public def clearDivPool():void{
		  commM.lmp.clear();		
	 }
	 
	 public def diversify():void{
		  //if (here.id < nTeams) commM.restartPool(); // clear pool on head node
		  
		  //val option = 1;
		  //if (option == 0){
				//Console.OUT.println("Force Restart");
				solver.forceRestart();
		  //}
		  //if (option == 1){
				//Console.OUT.println("Force Reset");
			//	solver.forceReset();
		 // }
	 }	
}
public type PlacesMultiWalks(s:Long)=PlacesMultiWalks{self.sz==s};
