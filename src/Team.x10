import x10.util.Random;

public class Team {
	val solverArray : Rail[ASSolverPermutSM];
	val cspArray : Rail[ModelAS];

	val region : Region(1);
	val stats : CSPStats;
	
	val updateI : Int;
	
	/**
	 *  Global Memory Shared between activities (Not used- replaced by static object)
	 */
	val sharedData : CommData;
	val sharedDataRef : GlobalRef[CommData];
	
	/**
	 *  Shared reference for inter team (place) communication 
	 */
	val interTeamSharedData : CommData;
	val interTeamSharedDataRef : GlobalRef[CommData];
	var arrayRefs : Rail[GlobalRef[CommData]];
	
	val commOption : Int;
	val poolSize : Int;
	val nbExplorerPT : Int;
	
	static val eliteP : ElitePool = new ElitePool ();
		
	def this (upI : Int, commOpt : Int , ps : Int, nbExPT : Int){
		
		commOption = commOpt;
		poolSize = ps;
		nbExplorerPT = nbExPT;
		updateI = upI;
		
		region = 0..(nbExplorerPT-1);
		solverArray = new Rail[ASSolverPermutSM](region);
		cspArray = new Rail[ModelAS](region);
		stats = new CSPStats();
		
		//Intra-Team communication
		sharedData = new CommData(poolSize); 
		sharedDataRef = GlobalRef[CommData](sharedData);
		
		//Inter-Team communication
		interTeamSharedData = new CommData(1);
		interTeamSharedDataRef = GlobalRef[CommData](interTeamSharedData); //Inter-Team communication
		arrayRefs = new Rail[GlobalRef[CommData]](0..((Place.MAX_PLACES)-1));
		
		Team.eliteP.clear();
		Team.eliteP.poolSize = poolSize;
	}
	
	def solve(size : Int , cspProblem : Int ) : Int{
		
		//var extTime : Long = -System.nanoTime();
		val random = new Random();
		finish for(aID in region){ 
			//Console.OUT.println("id= "+aID(0));
			//val size = 20;
			//val cspProblem = 1; 
			async{
				val seed = random.nextLong();
				var nsize:Int = size;
				//val fakeSeed = seed1;
				//val seed = here.id as Long;
				if (cspProblem == 1) {			//Magic-Square
					nsize = size*size; 
					cspArray(aID) = new MagicSquareAS(size, seed);
				}else if(cspProblem == 2)  		//Costas
					cspArray(aID) = new CostasAS(size, seed);
				else if (cspProblem == 3) 		//All-Intervals
					cspArray(aID) = new AllIntervalAS(size, seed, true);
				else if (cspProblem == 4){		//Langford
					nsize = size*2;
					cspArray(aID) = new LangfordAS(size, seed);
				}else if (cspProblem == 5){ 		//All-Intervals
					cspArray(aID) = new PartitAS(size, seed);
				}
				
				solverArray(aID) = new ASSolverPermutSM(nsize, seed, 
						new ASSolverConf(ASSolverConf.USE_PLACES, sharedDataRef, updateI, commOption, poolSize, nbExplorerPT));
				
				val cost = solverArray(aID).solve(cspArray(aID));
				
				if (cost == 0){
					for (k in region) if (aID != k) async {
						solverArray(k).kill = true;
					}
					// Store info in global memory
					setStats(aID(0));
					//extTime += System.nanoTime();
					//Console.OUT.println("time "+here+" =" + extTime/1e9);
				}
			
			}
		}
		//extTime += System.nanoTime();
		//Console.OUT.println("time=" + extTime/1e9);
		//stats.time = extTime/1e9;
		return stats.cost;
	}
	
	def setStats(actID : Int  ){
		
		val winAct = actID;
		val tCost = solverArray(winAct).total_cost;
		//val time = (timeDist(winPlace))/1e9;
		val iters = solverArray(winAct).nbIterTot;
		val locmin = solverArray(winAct).nbLocalMinTot;
		val swaps = solverArray(winAct).nbSwapTot;
		val reset = solverArray(winAct).nbResetTot;
		val same = solverArray(winAct).nbSameVarTot;
		val restart = solverArray(winAct).nbRestart;
		val change = solverArray(winAct).nbChangeV;
		
		stats.setStats(tCost, here.id, winAct , 0, iters, locmin, swaps, reset, same, restart, change);
		//stats.print(99);
		//val winstats = new CSPStats
	}	
	//atomic static def itraTeamComm (){
	//}

}

