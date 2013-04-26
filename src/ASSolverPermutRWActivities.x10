/** ASSolverPermutRWActivities is a parallel implementation of Adaptive Search solver
 * 	in the x10 language. This implementation use distributed isolated intances
 * 	of the solver, each one with a diferent seeds in order to have differents 
 * 	scanning walks in the search space.
 *
 *	This implementation distribute the solver instances in Activities (threads)
 * 	
 * @author Danny Munera
 *  @version 0.1 	9 April, 2013  -> Fist Version
 * 					10 April, 2013 -> Changes queens by costas problem
 * 					12 April, 2013 -> TLP support
 */
import x10.util.Random;
public class ASSolverPermutRWActivities {
	val solverArray : Array[ASSolverPermut];
	val cspArray : Array[ModelAS];
	val timeArray : Array[Long];
	val nbAct :Int; 
	val region : Region(1);
	
	val stats : CSPStats;
	
	val updateI : Int;
	
	/**
	 * 	Constructor of the class
	 * 	@param upI Update interval (for communication)
	 *  @param nbActivities Number de activities to use (Must be accord to X10_NTHREADS environ var)
	 */
	def this( upI : Int, nbActivities : Int){
		nbAct = nbActivities;
		region = 0..(nbAct-1);
		solverArray = new Array[ASSolverPermut](region);
		cspArray = new Array[ModelAS](region);
		timeArray = new Array[Long](region, 0);
		updateI = upI;
		
		stats = new CSPStats();
	}
	
	/** 
	 * 	Solve the csp problem with nbAct instances of AS solver (With Threads)
	 * 	The first one that reach a valid solution send a kill to the others
	 * 	to finish the process.
	 * 	@param size size of the csp problem
	 * 	@param cspProblem code with the problem to be solved (1 for Magic Square Problems, other number for Queens Problem)
	 * 	@return cost of the solution
	 */
	public def solve( size : Int , cspProblem : Int ) : CSPStats{ 
		val random = new Random();
		
		finish for(aID in region){ 
			val seed = random.nextLong();
			
			async{
				var cost:Int = x10.lang.Int.MAX_VALUE; 
				var nsize:Int = size;
				
				if (cspProblem == 1) {			// Magic-Square
					nsize = size*size;
					cspArray(aID) = new MagicSquareAS(size, seed);
				}else if(cspProblem == 2)  		// Costas
					cspArray(aID) = new CostasAS(size, seed);
				else  							// All-Intervals
					cspArray(aID) = new AllIntervalAS(size, seed, true);
				
				solverArray(aID) = new ASSolverPermut(nsize, seed, updateI);
				
				timeArray(aID) = -System.nanoTime();
				cost = solverArray(aID).solve(cspArray(aID));
				timeArray(aID) += System.nanoTime();
				
				if (cost == 0){
					for (k in region) if (aID != k) async {
						solverArray(k).kill = true;
					}
					setStats(aID(0));
				}
			}
		}
		return stats;
	}
	
	def setStats( id : Int){
		val winPlace = id;
		val time = (timeArray(winPlace))/1e9;
		val iters = solverArray(winPlace).nbIterTot;
		val locmin = solverArray(winPlace).nbLocalMinTot;
		val swaps = solverArray(winPlace).nbSwapTot;
		val reset = solverArray(winPlace).nbResetTot;
		val same = solverArray(winPlace).nbSameVarTot;
		val restart = solverArray(winPlace).nbRestart;
		
		stats.setStats(winPlace, time, iters, locmin, swaps, reset, same, restart);
		//val winstats = new CSPStats
	}
	

}

