package csp.solver; 
import csp.util.Logger;
import csp.model.ModelAS;
import x10.util.Random;
import x10.util.StringUtil;
import csp.util.Utils;
import csp.model.ParamManager;
import x10.io.File;
import x10.io.Printer;
/**	This class containts all the basic CommManager configuration info, for the
 * 	Adaptive search solver x10 implementation ASSolverPermut
 * 	
 * 	@autor Danny Munera
 * 	
 * Every place has an ASSolverPermutRW. This points to an CommManager.
 * comm is stored in ASSolverPermutRW.
 */
public class CommManager(sz:Long) {
	 
	 // id of the node used to manage the Local Min Pool
	 //public static LOCAL_MIN_NODE = 1;
	 public static LOCAL_MIN_NODE = Place.MAX_PLACES-1;
	 
	 public static USE_PLACES  = 0n;
	 public static USE_ACTIVITIES  = 1n; 
	 
	 public static NO_COMM = 0n;
	 public static ALL_TO_ZERO = 1n;
	 public static ALL_TO_NEIGHBORS = 3n;
	 public static ALL_TO_ALL = 2n;
	 public static TEAM = 4n;
	 
	 
	 // Elite pool
	 val ep:SmartPool(sz);
	 val epSize:Int;
	 // Local Min pool
	 val lmp:SmartPool(sz);
	 val lmpSize:Int;
	 
	 // Solver use activities or places 
	 var solverMode : Int;
	 // Number of iterations between each communication activity
	 var inTeamReportI : Int;
	 var inTeamUpdateI : Int;
	 // Number of iterations between each communication activity
	 var interTI : Int;
	 var delta : Int=0n;
	 val nTeams : Int;
	 val myTeamId : Long;
	 var random :Random = new Random();
	 val changeProb:Int;
	 // reference to team members, communication.
	 val solvers:PlaceLocalHandle[IParallelSolver(sz)];
	 
	 val isHeadNode:Boolean;
	 
	 // Max number of steps in PR 
	 private var ns:Int;
	 private var deltaFact : Double = 1.0;
	 private var pSendLM:Double = 0.0;
	 
	 private val divOption:Int;
	 
	 // Report status in alternative tty
	 private val altTty:File;
	 val p:Printer;
	 private val debug:Boolean;
	 
	 
	// val divFn:( vector : Rail[Int]{self.size==sz})=>Boolean; 
	 
	 def this(sz:Long, opts:ParamManager, ss: PlaceLocalHandle[IParallelSolver(sz)], nTeams:Int ){
		  property(sz);
		  this.solvers = ss;
		  this.epSize = opts("P_e", 4n);
		  this.lmpSize = opts("P_lm", 4n);
		  
		  val epM = opts("P_eM", 0);
		  val lmM = opts("P_lmM", 0);
		  val epD = opts("P_eD", 0.5);
		  val lmD = opts("P_lmD", 0.5);
		  
		  this.divOption = opts("O", 0n);
		  var divs:String;
		  if (divOption == 0n)
				divs = "from scratch";
		  else if (divOption == 1n)
				divs = "path relinking based";
		  else if (divOption == 2n)
				divs = "divTS based";
		  else //if (divOption == 3n)
				divs = "Random";
		  
		  if(here.id == 0){
				Console.OUT.println("Elite Pool Parameters - Size "+epSize+" mode "+epM+(epM==1?"":(" Dist "+epD)));
				Console.OUT.println("LM Pool Parameters - Size "+lmpSize+" mode "+lmM+(lmM==1?"":" Dist "+lmD));
				Console.OUT.println("Diversification Technique: "+divs);
		  }
		  
		  this.ep = new SmartPool(sz, epSize, epM, epD); 
		  this.lmp = new SmartPool(sz, lmpSize, lmM, lmD); 
		  // solver mode is always place TODO
		  this.solverMode = 0n;
		  
		  this.inTeamReportI = opts("-R", 0n);;
		  this.inTeamUpdateI = opts("-U", 0n);;
		  this.changeProb = opts("-C", 100n);
		  
		  this.nTeams = nTeams;
		  this.myTeamId = here.id % nTeams;
		  val m = myTeamId; val s = solverMode;
		  this.isHeadNode = here.id == myTeamId;
		  
		  Logger.debug(()=>{(s==0n ? ("My team is: " + m):("My team is:"+here.id))});
		  
		  val str = System.getenv("DELTA");
		  if (str != null)
				deltaFact = StringUtil.parseInt(str)/ 100.0;
		  
		  val nsStr = System.getenv("NS");
		  if (nsStr != null) 
				ns = StringUtil.parseInt(nsStr);
		  else
				ns = this.sz as Int / 4n;
		  
		  val lmstr = System.getenv("LM");
		  if (lmstr != null)
				pSendLM = StringUtil.parseInt(lmstr)/ 100.0;
		  
		  
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
		  
		  // val dbg = opts("-dbg", 0);
		  // this.debug = dbg == 1;
		  // this.altTty = new File("/dev/pts/1");
		  // this.p = altTty.printer(); 
		  
		  if (debug && here.id == 0){
				p.print("\033[2J\033[H");
				p.printf("Debug \n");
				for (i in 1..nTeams)
					 p.printf("Team %3d          best cost \n",i);
		  }
	 }
	 
	 public def setSeed(seed:Long){
		  random = new Random(seed);
		  ep.setSeed(random.nextLong());
		  lmp.setSeed(random.nextLong());
	 }
	 
	 public def setValues(toSet: CommManager{self.sz==this.sz}){
		  this.solverMode = toSet.solverMode;
		  this.inTeamReportI = toSet.inTeamReportI;
		  this.inTeamUpdateI = toSet.inTeamUpdateI;
		  this.interTI = toSet.interTI;
	 }
	 
	 /**
	  * 	communicate the vector if Searching thread totalCost is better than worstCost in the pool
	  * 
	  */
	 public def communicate(totalCost : Long, variables : Rail[Int]{self.size==sz} ) {
		  Logger.debug(()=>" communicate: entering.");
		  val placeid = here.id as Int;
		  val ss = solvers;
		  if (solverMode == USE_PLACES) {
				/************************** Comm Places *******************************/
				//Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
				Logger.debug(()=>"CommManager: solver mode -> Places.");
				
				//val variables = csp.variables; 
				
				if (Place(myTeamId)==here){
					 Logger.debug(()=>"CommManager: try to insert in local place: "+here);
					 ep.tryInsertConf( totalCost , variables, placeid);
				}else{
					 Logger.debug(()=>"CommManager: try to insert in remote place: "+Place(myTeamId));
					 at(Place(myTeamId)) async ss().tryInsertConf( totalCost , variables, placeid);
				}
				
				if (this.debug && this.isHeadNode){
					 val bc = ep.getBestConf();
					 if (bc != null){
						  p.print("\033[H\033["+(myTeamId+1)+"B");
						  p.printf("\033[2K\rTeam %3d          best cost %10d",myTeamId,bc().cost);
						  p.flush();
					 }
				}
				
				//Debug
				// if(here.id as Int == myTeamId ){ //group head
				//  Console.OUT.println("I'm "+myTeamId+" head group, here my ELITE pool Vectors");
				// ep.printVectors();
				// }
				/*********************************************************/
		  }else if (solverMode == USE_ACTIVITIES){
				Logger.debug(()=>"CommManager: solver mode: Activities.");
				Logger.debug(()=>"CommManager: try to insert in local place. ");
				ep.tryInsertConf( totalCost , variables, placeid);
		  }else{
				Console.OUT.println("ERROR: Unknown solver mode");
		  }
		  return;
	 }
	 
	 /**
	  *  Receive Local Minimum Confs. from Team member
	  */
	 public def communicateLM(totalCost : Long, variables : Rail[Int]{self.size==sz} ) {
		  Logger.debug(()=>" communicate: entering.");
		  
		  // decrease the number of vectors send it to the pool
		  if (random.nextDouble() >= pSendLM) return;		
		  
		  val placeid = here.id as Int;
		  val ss = solvers;
		  if (solverMode == USE_PLACES) {
				/************************** Comm Places *******************************/
				//Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
				Logger.debug(()=>"CommManager: solver mode -> Places.");
				
				//val variables = csp.variables; 
				
				if (Place(myTeamId) == Place(this.LOCAL_MIN_NODE)){
					 Logger.debug(()=>"CommManager: try to insert in local place: "+here);
					 lmp.tryInsertConf( totalCost , variables, placeid);
				}else{
					 Logger.debug(()=>"CommManager: try to insert in remote place: "+Place(myTeamId));
					 at(Place(this.LOCAL_MIN_NODE)) ss().tryInsertLM( totalCost , variables, placeid);
				}
				//Debug
				// if(here.id == LOCAL_MIN_NODE ){ //group head
				//   	Console.OUT.println("I'm "+myTeamId+" head group, here my Local MIN pool Vectors");
				//   	lmp.printVectors();
				// }
				// 
				if (this.debug && here.id == LOCAL_MIN_NODE){
					 val s = lmp.getCostList();
					 p.print("\033[H\033["+(nTeams+1)+"B");
					 p.print("\033[2K\rDiv Pool Costs: "+s);
					 p.flush();
				}
				
				/*********************************************************/
		  }else if (solverMode == USE_ACTIVITIES){
				Logger.debug(()=>"CommManager: solver mode: Activities.");
				Logger.debug(()=>"CommManager: try to insert in local place. ");
				ep.tryInsertConf( totalCost , variables, placeid);
		  }else{
				Console.OUT.println("ERROR: Unknown solver mode");
		  }
		  return;
	 }
	 
	 /**
	  *  get Inter Place Vector.This should be considered to have 
	  * modified csp_ in place, if the return value is 1n (success).
	  * If the return value is -1n (fail), csp_ will not be changed.
	  * 
	  */
	 public def getIPVector(csp_ : ModelAS(sz), myCost : Long):Boolean { // csp renamed csp_ to avoid issue with codegen in managed backend
		  // if (commOption == NO_COMM) return false;
		  Logger.debug(()=> "CommManager: getIPVector: entering.");
		  var a : Maybe[CSPSharedUnit(sz)];
		  if (solverMode == USE_PLACES) {
				Logger.debug(()=>"CommManager: getIPVector solver mode: Places.");
				val place = Place(myTeamId); // get reference to HEAD node
				val ss=solvers;
				if (place == here )
					 a = ep.getPConf();
				else{
					 a = at(place) ss().getConf();
				}
				//if (place.id==0)Console.OUT.println(here+" comm to "+place+" and get "+a().cost);
		  }else if (solverMode == USE_ACTIVITIES){
				Logger.debug(()=>"CommManager: getIPVector solver mode: Act.");
				a = ep.getPConf();
		  }else{
				a = null;
				Console.OUT.println("ERROR: Unknown solver mode");
		  }
		  // if ( a!=null && (myCost + delta) > a().cost &&  random.nextInt(100n) < changeProb ){
		  if ( a!=null && myCost  > a().cost * deltaFact &&  random.nextInt(100n) < changeProb ){
				csp_.setVariables(a().vector);
				return true; 
		  }
		  return false;
	 }
	 
	 
	 // public def getEPConf():Maybe[CSPSharedUnit(ep.sz)]{
	 // 	return ep.getPConf();
	 // }
	 // 
	 // public def getLMPConf():Maybe[CSPSharedUnit(lmp.sz)]{
	 // 	 return lmp.getPConf();
	 // }
	 
	 /** 
	  *  get a vector from the Local Min. Pool
	  * 
	  */ 
	 public def getLM( vector : Rail[Int]{self.size==sz}):Boolean { 
		  Logger.debug(()=> "CommManager: getLM: entering.");
		  var a : Maybe[CSPSharedUnit(sz)];
		  if (solverMode == USE_PLACES) {
				Logger.debug(()=>"CommManager: getLM solver mode: Places.");
				val place = Place(myTeamId);
				val ss = solvers;
				
				if (place == Place(this.LOCAL_MIN_NODE) )
					 a = lmp.getPConf();
				else{
					 a = at(Place(this.LOCAL_MIN_NODE)) ss().getLMConf();
				}
				//if (place.id==0)Console.OUT.println(here+" comm to "+place+" and get "+a().cost);
		  }else{
				a= null;
				Console.OUT.println("ERROR: Unknown solver mode");
		  }
		  if (a != null){
				Rail.copy(a().vector,vector);
				//csp_.setVariables(a().vector);
				return true; 
		  }
		  return false;
	 }
	 
	 
	 /**
	  * get a mutated vector using Path-Relinking based approach
	  * 
	  */
	 public def getPR( vector : Rail[Int]{self.size==sz}):Boolean { 
		  
		  var opt:Int = divOption;
		  if (divOption == 3n)
				opt = random.nextInt(3n);
		  
		  
		  if (opt == 0n) //Restart from Scratch
				return false;
		  else  if (opt == 1n)   // Restart PR-based
				return getPR1(vector);
		  else // opt == 2   // Restart using divTS
				return getPR2(vector);

	 }
	  
	 public def getPR0( vector : Rail[Int]{self.size==sz}):Boolean { 
		  return false;
	 }
	 
	 /**
	  * get a mutated vector using Path-Relinking based approach
	  * 
	  */
	 public def getPR1( vector : Rail[Int]{self.size==sz}):Boolean { 
		  Logger.debug(()=> "CommManager: getPR: entering.");
		  
		  // PATH RELINKING-based approach
		  val a = new Rail[Int](sz, 0n);
		  val b = new Rail[Int](sz, 0n);
		  val c = new Rail[Int](sz, 0n);
		  
		  val geta = this.getLM(a);
		  val getb = this.getLM(b);

		  if(geta && getb){
				// Utils.show("a=",a);
				// Utils.show("b=",b);
				Rail.copy(a, c);
				val nSteps = random.nextLong(ns);
				for(i in 0..nSteps) {
					 val bi = random.nextLong(sz);
					 val bval = b(bi);
					 var ci:Long = -1;
					 // search bval in vector a
					 for (cit in a.range()){
						  if (c(cit) == bval){
								ci = cit;
								break;
						  }
					 }
					 // swap variables
					 if(bi != ci){
						  //steps++;
						  val tmp = c(bi);
						  c(bi) = c(ci);
						  c(ci) = tmp;
						  // Utils.show("c=", c);
					 }
				}
				
				Rail.copy(c,vector);
				return true;
		  }else
				return false;
	 }
	 
	 /**
	  * get a diversified vector using Div technique by Glover 
	  * "A template fir scatter search and path relinking" 1998
	  * 
	  */
	 public def getPR2( vector : Rail[Int]{self.size==sz}):Boolean { 
		  Logger.debug(()=> "CommManager: getPR2: entering.");
		  
		  val seedConf = new Rail[Int](sz, 0n);
		  val finalConf = new Rail[Int](sz, 0n);
		  
		  val getSeedC = this.getLM(seedConf);
		  var position:Long = 0;
		  
		  if(getSeedC){
				val step = random.nextLong(sz/4) + 1;
				//val step = 2;
				//Utils.show("seed conf=",seedConf);
				//Console.OUT.println("step = " + step);
				for(var start:Long = step; start > 0; start--) {
					 for(var j:Long = start; j <= sz; j += step) {
						  //Console.OUT.println("j = " + j);		  
						  finalConf(position++) = seedConf(j-1);
					 }
				}
				Rail.copy(finalConf,vector);
				//Utils.show("final conf=",finalConf);
				
				return true;
		  }else
				return false;
	 }
	 
	 public def restartPool(){
		  Logger.debug(()=>"CommManager: clear Pool.");
		  ep.clear();
		  lmp.clear();
	 }
	 
	 
	 
}
public type CommManager(s:Long)=CommManager{self.sz==s};
