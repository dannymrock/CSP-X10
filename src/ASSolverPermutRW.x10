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
	val commEnable : Int;
	
	var bcost : Int;
	val stats : CSPStats;
	val refStats : GlobalRef[CSPStats];
	
	/** Comunication Variables*/
	val currentCosts : DistArray[Int];
	var commData : CommData;
	val refComm : GlobalRef[CommData];
	//val fileQAP : String;
	//val solverRef : GlobalRef[ASSolverPermutRW];
	
	/**
	 * 	Constructor of the class
	 */
	def this( upI : Int, commE : Int ){
		solverDist = DistArray.make[ASSolverPermut](Dist.makeUnique());
		cspDist = DistArray.make[ModelAS](Dist.makeUnique());
		timeDist = DistArray.make[Long](Dist.makeUnique());
		
		currentCosts = DistArray.make[Int](Dist.makeUnique(), -1);
		commData = new CommData(); 
		
		updateI = upI; 
		commEnable = commE;
		
		stats = new CSPStats();
		refStats = GlobalRef[CSPStats](stats);
		refComm = GlobalRef[CommData](commData);
		
		//fileQAP = file;
		
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
		
		
		val random = new Random();
		finish for(p in Place.places()){ 
				
			val seed = random.nextLong();
			
			at(p) async {
				var cost:Int = x10.lang.Int.MAX_VALUE; 
				var nsize:Int = size;
				
				if (cspProblem == 1) {			// Magic-Square
					nsize = size*size;
					cspDist(here.id) = new MagicSquareAS(size, seed);
				}else if(cspProblem == 2)  		// Costas
					cspDist(here.id) = new CostasAS(size, seed);
				else if (cspProblem == 3) 		// All-Intervals
					cspDist(here.id) = new AllIntervalAS(size, seed, true);
				else if (cspProblem == 4){		// Langford
					nsize = size*2;
					cspDist(here.id) = new LangfordAS(size, seed);
				}
				// else if (cspProblem == 5){ 	//QAP
				// 	val qapT = new QAPTools(fileQAP);
				// 	val sizeQAP = qapT.getSize();
				// 	nsize = sizeQAP;
				// 	cspDist(here.id) = new QAPAS(sizeQAP, seed, fileQAP);
				// }
				
				solverDist(here.id) = new ASSolverPermut(nsize, seed, 
							new ASSolverConf(ASSolverConf.USE_PLACES, refComm, updateI, commEnable ));
	
				timeDist(here.id) = -System.nanoTime();
				cost = solverDist(here.id).solve(cspDist(here.id));
				timeDist(here.id) += System.nanoTime();
				
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
		}
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
		
		at(refStats) refStats().setStats(winPlace, time, iters, locmin, swaps, reset, same, restart);
		//val winstats = new CSPStats
	}
	
	
	// public def setParameters(){
	// 	
	// }
	
	
	public def clear(){
		commData.clear();
	}
}
