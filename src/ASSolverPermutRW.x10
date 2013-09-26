/** ASSolverPermutRW is the parallel implementation of Adaptive Search solver
 * 	in the x10 language. This implementation use distributed isolated intances
 * 	of the solver, each one with a diferent seeds in order to have differents 
 * 	scanning walks in the search space.
 * 
 *  This implementation distribute the solver instances in Places
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013  -> Fist Version
 * 					10 April, 2013 -> Changes queens by costas problem
 * 					12 April, 2013 -> TLP support
 */
import x10.util.Random;
//import x10.compiler.Pragma;

public class ASSolverPermutRW{  
	val solverDist : DistArray[ASSolverPermut];
	val cspDist : DistArray[ModelAS];
	val timeDist : DistArray[Long];
	var winPlace : Place;
	
	val updateI : Int;
	val commOption : Int;
	
	var bcost : Int;
	val stats : CSPStats;
	val refStats : GlobalRef[CSPStats];
	
	/** Comunication Variables*/
	val currentCosts : DistArray[Int];
	var commData : CommData;
	val refComm : GlobalRef[CommData];
	//val fileQAP : String;
	//val solverRef : GlobalRef[ASSolverPermutRW];
	
	//All to all comm
	//val commDist : DistArray[CommData];
	//val refCommDist : GlobalRef[DistArray[CommData]];
	
	val poolSize : Int;
	
	
	val thEnable : Int; 
	
	//Hybrid approach
	val noGroups : Int;
	val sizeGroup : Int;
	//val refAllGroups :  Array[Rail[GlobalRef[CommData]]](1);
	
	/**
	 * 	Constructor of the class
	 */
	def this( upI : Int, commOpt : Int , thread : Int , ps : Int, nG : Int ){
		solverDist = DistArray.make[ASSolverPermut](Dist.makeUnique());
		cspDist = DistArray.make[ModelAS](Dist.makeUnique());
		timeDist = DistArray.make[Long](Dist.makeUnique());
		
		currentCosts = DistArray.make[Int](Dist.makeUnique(), -1);
		poolSize = ps;
		commData = new CommData( poolSize ); 
		
		updateI = upI; 
		commOption = commOpt;
		
		stats = new CSPStats();
		refStats = GlobalRef[CSPStats](stats);
		refComm = GlobalRef[CommData](commData);
		
		thEnable = thread;
		//fileQAP = file;
		
		
		//commDist = DistArray.make[CommData](Dist.makeUnique());
		//refCommDist = GlobalRef[DistArray[CommData]] (commDist);
		
		noGroups = nG; // will be a parameter 
		sizeGroup = Place.MAX_PLACES / noGroups ;
		
		//refAllGroups = new Array[Rail[GlobalRef[CommData]]](0..(noGroups-1));
		
		Console.OUT.println("There are "+noGroups+" groups each with "+sizeGroup+" nodes.");
	}
	
	/** 
	 * 	Solve the csp problem with MAX_PLACES instance of AS solver
	 * 	The first one that reach a valid solution send a kill to the others
	 * 	to finish the process.
	 * 	@param size size of the csp problem
	 * 	@param cspProblem code with the problem to be solved (1 for Magic Square Problems, other number for Queens Problem)
	 * 	@return cost of the solution
	 */
	public def solve( size : Int , cspProblem : Int ) : CSPStats{ 
		
		var extTime : Long = -System.nanoTime();
		val random = new Random();
		
		// Create solver and problem instances at each node
		finish for(p in Place.places()){ 
			val seed = random.nextLong();
			//val seed1 = 1234L;
			async at(p) async {
				var nsize:Int = size;
				//val fakeSeed = seed1;
				//val seed = here.id as Long;
				if (cspProblem == 1) {			//Magic-Square
					nsize = size*size; 
					cspDist(here.id) = new MagicSquareAS(size, seed);
				}else if(cspProblem == 2)  		//Costas
					cspDist(here.id) = new CostasAS(size, seed);
				else if (cspProblem == 3) 		//All-Intervals
					cspDist(here.id) = new AllIntervalAS(size, seed, true);
				else if (cspProblem == 4){		//Langford
					nsize = size*2;
					cspDist(here.id) = new LangfordAS(size, seed);
				}else if (cspProblem == 5){ 		//All-Intervals
					cspDist(here.id) = new PartitAS(size, seed);
				}
				// else if (cspProblem == 99){ 	//QAP
				// 	val qapT = new QAPTools(fileQAP);
				// 	val sizeQAP = qapT.getSize();
				// 	nsize = sizeQAP;
				// 	cspDist(here.id) = new QAPAS(sizeQAP, seed, fileQAP);
				// }
				
				if (thEnable == 0){
					solverDist(here.id) = new ASSolverPermut(nsize, seed, 
							new ASSolverConf(ASSolverConf.USE_PLACES, refComm, updateI, commOption, poolSize, noGroups ));
				}
			}
		}
		
		//Getting comm reference of each node
		val arrayRefs = new Rail[GlobalRef[CommData]](0..((Place.MAX_PLACES)-1));
		for(p in Place.places()){
			arrayRefs(p.id) = at(p){solverDist(here.id).myCommRef};	
		}
		
		// var g : Int = 0;
		// for( g = 0 ; g < noGroups; g++ ) {
		// 	refAllGroups(g) = new Rail[GlobalRef[CommData]](0..(sizeGroup-1));
		// 	//place de cadaq grupo
		// 	for(p in Place.places()){
		// 		//val g =p.id % noGroups;
		// 		//refAllGroups(g,1) = at(p){solverDist(here.id).myCommRef};	
		// 	}
		// }
		
		finish for(p in Place.places())async at(p) async { 
			var cost:Int = x10.lang.Int.MAX_VALUE;
				
				Array.copy(arrayRefs, solverDist(here.id).solverC.arrayRefs);
				
				/***/
				
				//timeDist(here.id) = -System.nanoTime();
				cost = solverDist(here.id).solve(cspDist(here.id));
				//	timeDist(here.id) += System.nanoTime();
				
				if (cost==0){
					for (k in Place.places()) if (here.id != k.id) at(k) 
					async 
					{
						solverDist(here.id).kill = true;
					}
					winPlace = here;
					bcost = cost;
					setStats();
				}
			}
		extTime += System.nanoTime();
		stats.time = extTime/1e9;
		this.clear();
		return stats; 
	}
	
	def setStats(  ){
		val winPlace = here.id;
		val time = (timeDist(winPlace))/1e9;
		val iters = solverDist(winPlace).nbIterTot;
		val locmin = solverDist(winPlace).nbLocalMinTot;
		val swaps = solverDist(winPlace).nbSwapTot;
		val reset = solverDist(winPlace).nbResetTot;
		val same = solverDist(winPlace).nbSameVarTot;
		val restart = solverDist(winPlace).nbRestart;
		val change = solverDist(winPlace).nbChangeV;
		
		at(refStats) refStats().setStats(winPlace, time, iters, locmin, swaps, reset, same, restart, change);
		//val winstats = new CSPStats
	}
	
	
	// public def setParameters(){
	// 	
	// }
	
	
	public def clear(){
		commData.clear();
	}
}
