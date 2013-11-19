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
package csp.solver;

import csp.models.*;
import csp.utils.*;
import x10.util.Random;
import x10.array.*;
import x10.util.concurrent.AtomicBoolean; 


public class IndependentWalks{  
	val solverDist : DistArray_Unique[ASPermut];
	val cspDist : DistArray_Unique[ModelAS];
	
	var winPlace : Place;
	
	val updateI : Int;
	val commOption : Int;
	
	var bcost : Int;
	val stats : CSPStats;
	val refStats : GlobalRef[CSPStats];
	
	/** Comunication Variables*/
	var commData : ElitePool;
	val refComm : GlobalRef[ElitePool];
	
	val poolSize : Int;
	
	
	val thEnable : Int; 
	
	//Hybrid approach
	val nbExplorerPT : Int;
	val nTeams : Int;

	/**
	 * 	Constructor of the class
	 */
	public def this( upI : Int, commOpt : Int , thread : Int , ps : Int, npT : Int ){
		solverDist = new DistArray_Unique[ASPermut] ();
		cspDist = new DistArray_Unique[ModelAS]();
		
		poolSize = ps;
		commData = new ElitePool( poolSize ); 
		
		updateI = upI; 
		commOption = commOpt;
		
		// General stats in Place 0 
		stats = new CSPStats();
		refStats = GlobalRef[CSPStats](stats);
		
		refComm = GlobalRef[ElitePool](commData);
		
		thEnable = thread;
				
		nbExplorerPT = npT; 
		nTeams = Place.MAX_PLACES as Int / nbExplorerPT ;
	}
	
	/** 
	 * 	Spawn the csp problem with MAX_PLACES instance of AS solver
	 * 	The first one that reach a valid solution send a kill to the others
	 * 	to finish the process.
	 * 	@param size size of the csp problem
	 * 	@param cspProblem code with the problem to be solved (1 for Magic Square Problems, other number for Queens Problem)
	 * 	@return cost of the solution
	 */
	public def spawn( size : Int , cspProblem : Int ) : CSPStats{ 
		
		var extTime : Long = -System.nanoTime();
		val random = new Random();
		
		// Create solver and problem instances at each node
		finish for(p in Place.places()){ 
			val seed = random.nextLong();
			async at(p) async {
				var nsize:Int = size;
				if (cspProblem == 1n) {			//Magic-Square
					nsize = size*size; 
					cspDist(here.id) = new MagicSquareAS(size, seed);
				}else if(cspProblem == 2n)  		//Costas
					cspDist(here.id) = new CostasAS(size, seed);
				else if (cspProblem == 3n) 		//All-Intervals
					cspDist(here.id) = new AllIntervalAS(size, seed, true);
				else if (cspProblem == 4n){		//Langford
					nsize = size*2n;
					cspDist(here.id) = new LangfordAS(size, seed);
				}else if (cspProblem == 5n){ 		//All-Intervals
					cspDist(here.id) = new PartitAS(size, seed);
				}
								
				solverDist(here.id) = new ASPermut(nsize, seed, 
					new ASSolverConf(ASSolverConf.USE_PLACES, refComm, updateI,0n, commOption, poolSize, nTeams ));
				
				
			}
		}
		
		//Getting comm reference of each node
		val arrayRefs = new Rail[GlobalRef[ElitePool]] (Place.MAX_PLACES);
		for(p in Place.places()){
			arrayRefs(p.id) = at(p){solverDist(here.id).myCommRef};	
		}
		
		
		finish for(p in Place.places())async at(p) async { 
			var cost:Int = x10.lang.Int.MAX_VALUE;
			
			Rail.copy(arrayRefs, solverDist(here.id).solverC.arrayRefs);
			
			cost = solverDist(here.id).solve(cspDist(here.id));
			
			if (cost == 0n){
				
				val home = here.id;
				val winner = at (Place.FIRST_PLACE) announceWinner(home);
				
				if (winner) {
					//Logger.info("winner "+here);
					setStats();
					val sol = cspDist(here.id).variables;
					Utils.show("Solution is ", sol);
				}
			}
		}
		extTime += System.nanoTime();
		stats.time = extTime/1e9;
		this.clear();
		return stats; 
	}
	
	def setStats(){
		val winPlace = here.id;
		//val time = (timeDist(winPlace))/1e9;
		val iters = solverDist(winPlace).nbIterTot;
		val locmin = solverDist(winPlace).nbLocalMinTot;
		val swaps = solverDist(winPlace).nbSwapTot;
		val reset = solverDist(winPlace).nbResetTot;
		val same = solverDist(winPlace).nbSameVarTot;
		val restart = solverDist(winPlace).nbRestart;
		val change = solverDist(winPlace).nbChangeV;
		
		at(refStats) refStats().setStats(0n, winPlace as Int, 0n, 0n, iters, locmin, swaps, reset, same, restart, change,0n);
		//val winstats = new CSPStats
	}
	
	public def clear(){
		commData.clear();
	}
	
	val winnerLatch = new AtomicBoolean(false);
	
	public def announceWinner(p:Long):Boolean {
	 	val result = winnerLatch.compareAndSet(false, true);
		
		//Logger.info(()=> "announceWinner result=" + result + " for " + p + " this=" + this );
		if (result) {
			for (k in Place.places()) 
				if (p != k.id) 
					at(k) async solverDist(here.id).kill = true; //Why cannot use solverDist(k) here??
		}
	 	return result;
	}
	
}
