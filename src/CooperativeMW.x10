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
import x10.array.*; //Change for x10.array.*;

class CooperativeMW{  
	val teamDist : DistArray_Unique[Team];
	
	val intraTI : Int;
	val interTI : Int;
	
	val stats : CSPStats;
	val refStats : GlobalRef[CSPStats];
	
	val poolSize : Int;
	
	val thEnable : Int; 
	
	//Hybrid approach
	val nbExplorerPT : Int;
	val sizeGroup : Int;
	val minDistance : Double;
	
	/**
	 * 	Constructor of the class
	 */
	def this( intraTeamI : Int, interTeamI : Int , thread : Int , ps : Int, nExPT : Int, minD:Double){
		teamDist = new DistArray_Unique[Team]();
		
		poolSize = ps;
		
		intraTI = intraTeamI; 
		interTI = interTeamI;
		
		stats = new CSPStats();
		refStats = GlobalRef[CSPStats](stats);
		
		thEnable = thread;
				
		nbExplorerPT = nExPT; 		// will be a parameter 
		sizeGroup = nbExplorerPT ;
		minDistance = minD;
		
		
		//Console.OUT.println("Each Team with "+nbExplorerPT+" explorers.");
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
		
		// 1st step: Create team instances at each node
		finish for(p in Place.places()){ 
			val seed = random.nextLong();
			async at(p){
				teamDist(here.id) = new Team(intraTI, interTI, poolSize, nbExplorerPT, minDistance);	
			}
		}
		
		// 2nd step: Get comm references of each node
		val arrayRefs = new Rail[GlobalRef[Team]] (Place.MAX_PLACES);
		for(p in Place.places()){
			arrayRefs(p.id) = at(p){ GlobalRef[Team](teamDist(here.id)) };	
		}
		
		// 3rd step: Start solve process at each team
		finish for ( p in Place.places() ) at(p) async { 
			var cost:Int = x10.lang.Int.MAX_VALUE;
			
			//Passing all refs to each team
			Rail.copy(arrayRefs, teamDist(here.id).arrayRefs);
				
			//Starting solve at each team
			cost = teamDist(here.id).solve(size , cspProblem); //cspDist(here.id));
				
			if (cost == 0n){
				for (k in Place.places()) if (here.id != k.id) at(k) 
				async 
				{	
					//for (i in 0..(nbExplorerPT-1)){
						//teamDist(here.id).solverArray(i).kill = true;
					//}
					teamDist(here.id).control.exit = true;
					atomic{
						teamDist(here.id).control.event = true; 
					}
				}
				setStats();
			}
		}
		extTime += System.nanoTime();
		stats.time = extTime/1e9;
		//stats.print(98);
		return stats; 
	}
	
	def setStats(  ){
		val winTeam = here.id;
		val tCost = teamDist(winTeam).stats.cost;
		val winExp = teamDist(winTeam).stats.explorer;
		val iters = teamDist(winTeam).stats.iters;
		val locmin = teamDist(winTeam).stats.locmin;
		val swaps = teamDist(winTeam).stats.swaps;
		val reset = teamDist(winTeam).stats.reset;
		val same = teamDist(winTeam).stats.same;
		val restart = teamDist(winTeam).stats.restart;
		val change = teamDist(winTeam).stats.change;
		val forceR = teamDist(winTeam).stats.forceRestart;
		// Console.OUT.println(winTeam+" "+winExp+" "+time+" "+iters+" "+locmin+" "+swaps+" "+reset+" "+same+" "+restart+" "+change);
		at(refStats) refStats().setStats(tCost, winTeam as Int, winExp , 0n, iters, locmin, swaps, reset, same,
				restart, change, forceR);
	}
}
