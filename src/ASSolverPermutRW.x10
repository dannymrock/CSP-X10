/** ASSolverPermutRW is the parallel implementation of Adaptive Search solver
 * 	in the x10 language. This implementation use distributed isolated intances
 * 	of the solver, each one with a diferent seeds in order to have differents 
 * 	scanning walks in the search space.
 * 
 *  The AS solver Implementation is specialized in Permuts Problems and no exhaustive search.
 * 
 *  Based on the C implementation of Adaptive Search algoritm by Daniel Diaz
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
	var bcost : Int;
	val stats : CSPStats;
	val refStats : GlobalRef[CSPStats];
	/** Solver with TLP */
	val threadEnable : Int;
	 
	/**
	 * 	Constructor of the class
	 */
	def this( threadE : Int ){
		solverDist = DistArray.make[ASSolverPermut](Dist.makeUnique());
		cspDist = DistArray.make[ModelAS](Dist.makeUnique());
		timeDist = DistArray.make[Long](Dist.makeUnique());
		updateI = 10; //Can be a parameter
		stats = new CSPStats();
		refStats = GlobalRef[CSPStats](stats);
		threadEnable = threadE;
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
				else  							// All-Intervals
					cspDist(here.id) = new AllIntervalAS(size, seed);
				
				if (threadEnable == 0)
					solverDist(here.id) = new ASSolverPermut(nsize, seed, updateI);
				else
					solverDist(here.id) = new ASSolverPermutTLB(nsize, seed, updateI);
				
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
		return stats; //return cost (this is not good)
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
	
	
}