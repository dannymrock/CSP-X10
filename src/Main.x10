/** Main
 * 	Main class of the project. 
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013 	-> First Version
 */

import x10.io.File;
import x10.io.FileWriter;
import x10.util.OptionsParser;
import x10.util.Option;
import x10.util.Random;

public class Main {
	//static val paramObj = new Parameters();
	//static val param = GlobalRef[Parameters](new Parameters());
	
	public static def main(args:Rail[String]):void{
		
		/*
		 *  Parsing Options
		 */
		val opts = new OptionsParser(args, new Rail[Option](0L), [
		                                          Option("p", "", "Problem Selection (magic-square, costas, all-interval or langford)"),
		                                          Option("s", "", "Size of the problem"),
		                                          Option("b", "", "Number of benchmark tests"),
		                                          Option("m", "", "Solver mode distribution 0 for Places \"n\" for Activities (n number of activities). Default 0."),
		                                          Option("t", "", "Using threads."),
		                                          Option("c", "", "Communication option: 0 no comm 1 for \"place 0\", 2 for all-to-all and 3 for neighbors"),
		                                          Option("i", "", "Intra-team Communication Interval (iterations) . Default 0."),
		                                          Option("j", "", "Inter-team Communication Interval (iterations) . Default 0."),
		                                          Option("n", "", "nodes_per_team parameter. Default 4."),
		                                          Option("k", "", "poolsize."),
		                                          Option("d", "", "minimum permisible distance.")
		                                          ]);
		
		val cspProblem = opts("-p", "magic-square");
		val size = opts("-s", 10n);
		val testNo = opts("-b", 10n);
		val solverMode = opts("-m", 0n);
		val threads = opts("-t", 0n);
		val comm = opts("-c", 0n);
		val intraTI = opts("-i", 0n);
		val interTI = opts("-j", 0n);
		val nodesPTeam = opts("-n", 1n);
		val poolSize = opts("-k", 4n);
		val minDistance = opts("-d", 0.3);
		
		//at(Main.param) Main.param().poolSize = poolSize;

		Console.OUT.println("CSP Problem: "+cspProblem+" Size: "+size+"\nNumber of repetitions: "+testNo+
							"\nSolverMode: "+solverMode+"\nCommunication strategy: "+comm+
				            "\nIntra-Team Comm. inteval: "+intraTI+"\nInter-Team Comm. inteval: "+interTI+
				            "\nMinimum permissible distance: "+minDistance+
							"\nPool Size: "+poolSize);
		
		var param : Int = 0n;
		//var file : String = "";
				
		if (cspProblem.equals("magic-square")) {
			//Console.OUT.println("Magic Square Problem");
			param = 1n;
		}else if(cspProblem.equals("costas")){
			//Console.OUT.println("Costas Array Problem");
			param = 2n;
		}else if(cspProblem.equals("all-interval")){
			//Console.OUT.println("All-Interval Array Problem");
			param = 3n;
		}else if(cspProblem.equals("langford")){
			//Console.OUT.println("Langford Pairing Problem");
			param = 4n;
		}else if(cspProblem.equals("partit")){
			//Console.OUT.println("Number Partition Problem");
			param = 5n;
		}
	
		else{
			Console.OUT.println("Error: Type a valid CSP example: magic-square"); 
			return;
		}
		
		/*
		 *  Creating objects for solver execution
		 */
		var timeStart : Long;
		var cost : Int;
		var timeEnd : Long;
		var sumTimes: Long = 0L;
		val accStats = new CSPStats();
		
		// communication interval = 10
		val solverP = new ASSolverPermutRW(intraTI, comm, threads, poolSize, nodesPTeam); 
		val solverT = new CooperativeMW(intraTI, interTI, threads, poolSize, nodesPTeam, minDistance);

		if (solverMode == 0n){
			Console.OUT.println("Using multi-walks with "+Place.MAX_PLACES+" Places");
			Console.OUT.println("There are "+Place.MAX_PLACES+" teams each one with "+nodesPTeam+" explorer places. "+
					Place.MAX_PLACES*nodesPTeam+" explorers in total (places)");
		} else{
			Console.OUT.println("Using multi-walks with "+Place.MAX_PLACES+" places and "+nodesPTeam+" activities");
			Console.OUT.println("There are "+Place.MAX_PLACES+" teams each one with "+nodesPTeam+
					" explorer activities. "+Place.MAX_PLACES*nodesPTeam+" explorers in total (places and activities)");
		}
		
		Console.OUT.println("|Count| Time (s) |  Iters   | Place |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| Change|  FR |");
		Console.OUT.println("|-----|----------|----------|-------|----------|----------|----------|-------|-----|-------|-----|");
		
		/*
		 *  Execution loop
		 */
		for (var j : Int = 1n; j <= testNo ; j++ ){
			
			//Solve the problem
			val stats:CSPStats;
			if (solverMode == 0n){ 
				stats = solverP.solve(size,param);
			}else{
				stats = solverT.solve(size,param);
			}
			accStats.accStats(stats);
			Console.OUT.printf("\r");
			stats.print(j);
			accStats.printAVG(j);
			Console.OUT.flush();
			
			//clean solver
			
		}
		Console.OUT.printf("\r");
		Console.OUT.println("|-----|----------|----------|-------|----------|----------|----------|-------|-----|-------|-----|");
		accStats.printAVG(testNo);
		Console.OUT.printf("\n");
		
		return;
	}

	static def show(s:String, d: Rail[Int]) {
		Console.OUT.print(s + " in "+here.id+" : ");
		for(k in d) 
			Console.OUT.print(" " + d(k));		
		Console.OUT.println("");
	}
}


// class Parameters{
// 	var poolSize : Int;
// 	
// 	def this(){
// 		Console.OUT.println("solo una vez");
// 		poolSize = 2n;
// 	}
// 	
// 	
// } 