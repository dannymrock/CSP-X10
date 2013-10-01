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
	//val interTeamSharedData : CommData;
	//val interTeamSharedDataRef : GlobalRef[Team];
	
	
	val commOption : Int;
	val poolSize : Int;
	val nbExplorerPT : Int;
	
	var arrayRefs : Rail[GlobalRef[Team]];
	
	static val control : Control = new Control();
		
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
		//interTeamSharedData = new CommData(1);
		//interTeamSharedDataRef = GlobalRef[Team](this); //Inter-Team communication
		
		
		Team.control.clear();
		Team.control.poolSize = poolSize;
		
		arrayRefs = new Rail[GlobalRef[Team]](0..((Place.MAX_PLACES)-1));
		
	}
	
	def solve(size : Int , cspProblem : Int ) : Int{
		
		//var extTime : Long = -System.nanoTime();
		val random = new Random();
		finish{
			
			async{
				control();
			}
			
			for(aID in region){ 
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
					
					solverArray(aID) = new ASSolverPermutSM(aID(0), nsize, seed, 
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
						atomic{
							control.exit = true;
							control.event = true; 
						}
						
					}
				}//async
			}//for
		}//finish
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
	
	
	
	public def doIterTeamComm ( ){
		var nextPlace : Int = here.id + 1;
		
		if (nextPlace >= Place.MAX_PLACES )
			nextPlace = 0;
		
		// get a conf from any local explorer
		val myConf : Rail[Int] = cspArray(0).variables;
		val myCost : Int = solverArray(0).total_cost;
		
		// get a conf from any explorer in the next team
		val next = nextPlace;
		val nextConf : Rail[Int] = at(arrayRefs(next)) arrayRefs(next)().cspArray(0).variables;
		val nextCost : Int = at(arrayRefs(next)) arrayRefs(next)().solverArray(0).total_cost;
		
		// compute distance
		val dis = distance( myConf, nextConf );
		
		Console.OUT.println("distance " + dis);
		// if distance < mindistance
		// restart the team with greater cost
		// end if
		// trigget interTeam event in next place
		
	}
	
	def distance(conf1 : Rail[Int], conf2 : Rail[Int]) : Float {
		val sizeC = conf1.size;
		var i : Int = 0;
		var count : Int = 0;
		for (i = 0; i < sizeC; i++){
			if(conf1(i) == conf2(i)){
				count++; 
			}
		}
		val dis = 1 - ( count / sizeC );
		return dis;
	}
	
	def control(){
		var test : Boolean = true;
		var act : Int = 0;
		
		while ( test ) {
			
			Runtime.probe();
			
			when ( control.event ) {
				control.event = false;
			}
			
			if ( control.exit )
				test = false;
			else
				if ( control.interTeam ) {
					control.interTeam = false;
					//act = 1;
					doIterTeamComm();
					Console.OUT.println( "sending message" );
				}				
		
			// if ( act == 1 ) {
			// 	act = 0;
			// 	doIterTeamComm();
			// }
			//Runtime.probe();
		}
		Console.OUT.println( "exit control " + here );
	}
}

