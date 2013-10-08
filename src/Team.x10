import x10.util.Random;

public class Team {
	val solverArray : Rail[ASSolverPermutSM];
	val cspArray : Rail[ModelAS];

	val region : Region(1);
	val stats : CSPStats;
	
	val intraTI : Int;
	val interTI : Int;

	val commOption : Int;
	val poolSize : Int;
	val nbExplorerPT : Int;
	
	/**
	 *  Shared references for inter teams (places) communication 
	 */
	var arrayRefs : Rail[GlobalRef[Team]];
	
	var minDistance : Double;
	
	val random : Random;
	
	var count: Int;
	
	/**
	 *  Global Memory Shared between activities (static object)
	 */
	static val control : Control = new Control();
		
	def this (intraTeamI : Int, interTeamI : Int , ps : Int, nbExPT : Int, minD: Double){
		
		commOption = 0;
		poolSize = ps;
		nbExplorerPT = nbExPT;
		intraTI = intraTeamI;
		interTI = interTeamI;
		
		region = 0..(nbExplorerPT-1);
		solverArray = new Rail[ASSolverPermutSM](region);
		cspArray = new Rail[ModelAS](region);
		stats = new CSPStats();
		
		Team.control.clear();
		Team.control.poolSize = poolSize;
		
		arrayRefs = new Rail[GlobalRef[Team]](0..((Place.MAX_PLACES)-1));
		minDistance = minD;
		random =  new Random();
		count = 0;
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
					
					//minDistance = cspArray(aID).solverParams.minDistance;
					solverArray(aID) = new ASSolverPermutSM(aID(0), nsize, seed,
							new ASSolverConf( ASSolverConf.USE_PLACES, GlobalRef[CommData](null), intraTI, 
									interTI, commOption, poolSize, nbExplorerPT)
						);
					
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
		val iters = solverArray(winAct).nbIterTot;
		val locmin = solverArray(winAct).nbLocalMinTot;
		val swaps = solverArray(winAct).nbSwapTot;
		val reset = solverArray(winAct).nbResetTot;
		val same = solverArray(winAct).nbSameVarTot;
		val restart = solverArray(winAct).nbRestart;
		val change = solverArray(winAct).nbChangeV;
		val forceR = solverArray(winAct).nbForceRestart;
		
		stats.setStats(tCost, here.id, winAct , 0, iters, locmin, swaps, reset, same, restart, change, forceR);
		//stats.print(99);
	}	
	
	
	
	
	def control(){
		var test : Boolean = true;
		var act : Int = 0;
		loop: while ( true ) {
			//Runtime.probe();
			when ( control.event ) {
				control.event = false;
				count++;
				if ( control.exit )
					break loop;
				
				if ( control.interTeam ) {
					control.interTeam = false;
					act = 1;
					//doIterTeamComm();
					//Console.OUT.println(here+" C: put action "+count );
				}				
			}
			if ( act == 1 ) {
				act = 0;
				//Console.OUT.println(here+" C: starting inter team comm "+count );
				doIterTeamComm();
				//Console.OUT.println(here+" C: end inter team comm "+count);
			}
			Runtime.probe();
		}
		//Console.OUT.println( count+" exit control " + here );
	}
	
	public def doIterTeamComm (){// myConf : Rail[Int], myCost : Int ){
		// Compare against next team
		//val tmp : Int = here.id + 1 < Place.MAX_PLACES ?  here.id + 1 : 0;
		
		//Compare against random team
		var tmp : Int = random.nextInt(Place.MAX_PLACES);
		while (here.id == tmp){
			tmp = random.nextInt(Place.MAX_PLACES);
		}
		
		//val nextPlace = tmp; 
		val ref = arrayRefs(tmp);
		//var extTime : Long = -System.nanoTime();
	
		val conf1 = cspArray(0).variables;
		val cost1 = solverArray(0).total_cost;
		//extTime += System.nanoTime();
		//Console.OUT.println(here+" time0: "+extTime);
		
		//extTime = -System.nanoTime();
		
		val conf2 : Rail[Int] = at(ref) ref().cspArray(0).variables;
		val cost2 : Int = at(ref) ref().solverArray(0).total_cost;
		//extTime += System.nanoTime();
		
		//Console.OUT.println(here+" time1: "+extTime);
		// // get conf1 from any explorer in a random team
		// val place1 = random.nextInt(Place.MAX_PLACES);
		// //val next = nextPlace;
		// val conf1 : Rail[Int] = at(arrayRefs(place1)) arrayRefs(place1)().cspArray(0).variables;
		// val cost1 : Int = at(arrayRefs(place1)) arrayRefs(place1)().solverArray(0).total_cost;
		
		// get conf2 from any explorer in a random team
		// var tmp : Int = random.nextInt(Place.MAX_PLACES);
		// while (place1 == tmp){
		// 	tmp = random.nextInt(Place.MAX_PLACES);
		// }
		// val place2 = tmp;
		// //val next = nextPlace;
		// val conf2 : Rail[Int] = at(arrayRefs(place2)) arrayRefs(place2)().cspArray(0).variables;
		// val cost2 : Int = at(arrayRefs(place2)) arrayRefs(place2)().solverArray(0).total_cost;
		
		// compute distance
		val dis = distance( conf1, conf2 );
		if ( control.exit ) return;
		//Console.OUT.println("distance between "+ place1+" and "+ place2+" is: " + dis);
		//Console.OUT.println(" distance between "+ here.id+" and "+ nextPlace+" is: " + dis);
		
		// if distance < mindistance
		if (dis < minDistance){
			// restart the team with greater cost
			if ( cost1 > cost2 ){
				//restart place1
				// val x = at(arrayRefs(place1)) arrayRefs(place1)().solverArray.size; 
				// for(i in 0..(x-1))
				// 	at(arrayRefs(place1)) arrayRefs(place1)().solverArray(i).forceRestart = true;
				//----
				val x = solverArray.size; // /2; //restart only the first half in the explorer set
				for(i in 0..(x-1))
					solverArray(i).forceRestart = true;
			}else{
				//restart place2
				// val x = at(arrayRefs(place2)) arrayRefs(place2)().solverArray.size; 
				// for(i in 0..(x-1))
				// 	at(arrayRefs(place2)) arrayRefs(place2)().solverArray(i).forceRestart = true;
				//----
				at(ref){
					val x = ref().solverArray.size; // /2; //restart only the half 
					for(i in 0..(x-1))
						ref().solverArray(i).forceRestart = true;
				}
			}			
		}
		// start inter-team comm in next place (ring)
		// if ( control.exit ) return;
		// if (here.id < Place.MAX_PLACES-1) //last place don't send iter-team comm intention
		// 	at(ref){
		// 		atomic{
		// 			ref().control.interTeam = true;
		// 			ref().control.event = true;
		// 		}
		// 	}
	}
	
	def distance(conf1 : Rail[Int], conf2 : Rail[Int]) : Double {
		val sizeC = conf1.size;
		var i : Int = 0;
		var count : Int = 0;
		for (i = 0; i < sizeC; i++){
			//Console.OUT.println("comparing: "+conf1(i)+" - "+conf2(i));
			//if ( control.exit ) return 1.0;
			if(conf1(i) == conf2(i)){
				count++; 
			}
		}
		val dis = 1.0 - ( count as Double / sizeC );
		return dis;
	}	
}
