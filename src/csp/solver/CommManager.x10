package csp.solver; 
import csp.util.Logger;
import csp.model.ModelAS;
import x10.util.Random;
import x10.util.StringUtil;
import csp.util.Utils;
import csp.model.ParamManager;
/**	This class containts all the basic CommManager configuration info, for the
 * 	Adaptive search solver x10 implementation ASSolverPermut
 * 	
 * 	@autor Danny Munera
 * 	
 * Every place has an ASSolverPermutRW. This points to an CommManager.
 * comm is stored in ASSolverPermutRW.
 */
public class CommManager(sz:Long) {
	
	
	public static USE_PLACES  = 0n;
	public static USE_ACTIVITIES  = 1n; 
	
	public static NO_COMM = 0n;
	public static ALL_TO_ZERO = 1n;
	public static ALL_TO_NEIGHBORS = 3n;
	public static ALL_TO_ALL = 2n;
	public static TEAM = 4n;
	
	
	// Elite pool
	val ep:SmartPool;
	val epSize:Int;
	// Local Minimums pool
	val lmp:SmartPool;
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
	val myTeamId : Int;
	var random :Random = new Random();
	val changeProb:Int;
	var deltaFact : Double = 1.0;
	// reference to team members, communication.
	val solvers:PlaceLocalHandle[IParallelSolver(sz)];
	
	def this(sz:Long, opts:ParamManager, ss: PlaceLocalHandle[IParallelSolver(sz)], nTeams:Int ){
		property(sz);
		this.solvers = ss;
		this.epSize = opts("P_e", 4n);
		this.lmpSize = opts("P_lm", 4n);
		
		val epM = opts("P_eM", 0);
		val lmM = opts("P_lmM", 0);
		val epD = opts("P_eD", 0.5);
		val lmD = opts("P_lmD", 0.5);
		
		this.ep = new SmartPool(sz, epSize, epM, epD); 
		this.lmp = new SmartPool(sz, lmpSize, lmM, lmD); 
		// solver mode is always place TODO
		this.solverMode = 0n;
		
		this.inTeamReportI = opts("-R", 0n);;
		this.inTeamUpdateI = opts("-U", 0n);;
		this.changeProb = opts("-C", 100n);
		
		this.nTeams = nTeams;
		this.myTeamId = here.id as Int % nTeams;
		val m = myTeamId; val s = solverMode;
		Logger.debug(()=>{(s==0n ? ("My team is: " + m):("My team is:"+here.id))});
		
		val str = System.getenv("DELTA");
		if (str != null)
			 deltaFact = StringUtil.parseInt(str)/ 100.0;
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
			  //Debug
			  // if(here.id as Int == myTeamId ){ //group head
					// Console.OUT.println("I'm "+myTeamId+" head group, here my ELITE pool Vectors");
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
		 val placeid = here.id as Int;
		 val ss = solvers;
		 if (solverMode == USE_PLACES) {
			  /************************** Comm Places *******************************/
			  //Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
			  Logger.debug(()=>"CommManager: solver mode -> Places.");
			  
			  //val variables = csp.variables; 
			  
			  if (Place(myTeamId)==Place(1)){
					Logger.debug(()=>"CommManager: try to insert in local place: "+here);
					lmp.tryInsertConf( totalCost , variables, placeid);
			  }else{
					Logger.debug(()=>"CommManager: try to insert in remote place: "+Place(myTeamId));
					at(Place(1)) ss().tryInsertLM( totalCost , variables, placeid);
			  }
			  //Debug
			  // if(here.id == 1 ){ //group head
			  //   	Console.OUT.println("I'm "+myTeamId+" head group, here my Local MIN pool Vectors");
			  //   	lmp.printVectors();
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
	 *  get Inter Place Vector.This should be considered to have 
	 * modified csp_ in place, if the return value is 1n (success).
	 * If the return value is -1n (fail), csp_ will not be changed.
	 * 
	 */
	public def getIPVector(csp_ : ModelAS(sz), myCost : Long):Boolean { // csp renamed csp_ to avoid issue with codegen in managed backend
		 // if (commOption == NO_COMM) return false;
		 Logger.debug(()=> "CommManager: getIPVector: entering.");
		 var a : Maybe[CSPSharedUnit(ep.sz)];
		 if (solverMode == USE_PLACES) {
			  Logger.debug(()=>"CommManager: getIPVector solver mode: Places.");
			  val place = Place(myTeamId);
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
			  a= null;
			  Console.OUT.println("ERROR: Unknown solver mode");
		 }
		 //		if ( a!=null && (myCost + delta) > a().cost &&  random.nextInt(100n) < changeProb ){
		 if ( a!=null && myCost  > a().cost * deltaFact &&  random.nextInt(100n) < changeProb ){
			  //if ( a!=null && (myCost + delta) > a().cost &&  random.nextInt(100n) < 95){					 
			  csp_.setVariables(a().vector);
			  return true; 
		 }
		 return false;
	}
	
	
	public def getEPConf():Maybe[CSPSharedUnit(ep.sz)]{
		return ep.getPConf();
	}
	
	public def getLMPConf():Maybe[CSPSharedUnit(lmp.sz)]{
		 return lmp.getPConf();
	}
	
	/**
	 * 
	 */ 
	
	//public def getLM(csp_ : ModelAS(sz), myCost : Long):Boolean { // csp renamed csp_ to avoid issue with codegen in managed backend
	public def getLM( vector : Rail[Int]{self.size==sz}, myCost : Long):Boolean { 
		 
		  	 
		 
		 Logger.debug(()=> "CommManager: getLM: entering.");
		 var a : Maybe[CSPSharedUnit(sz)];
		 if (solverMode == USE_PLACES) {
			  Logger.debug(()=>"CommManager: getLM solver mode: Places.");
			  val place = Place(myTeamId);
			  val ss = solvers;
			  
			  if (place == Place(1) )
					a = lmp.getPConf();
			  else{
					a = at(Place(1)) ss().getLMConf();
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
	
	public def restartPool(){
		Logger.debug(()=>"CommManager: clear Pool.");
		ep.clear();
		lmp.clear();
	}
	
	
	
}
public type CommManager(s:Long)=CommManager{self.sz==s};
