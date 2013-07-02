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
	public static def main(args:Array[String](1)):void{
		
		/*
		 *  Parsing Options
		 */
		val opts = new OptionsParser(args, null, [
		                                          Option("p", "", "Problem Selection (magic-square, costas, all-interval or langford)"),
		                                          Option("s", "", "Size of the problem"),
		                                          Option("b", "", "Number of benchmark tests"),
		                                          Option("m", "", "Solver mode distribution 0 for Places \"n\" for Activities (n number of activities). Default 1."),
		                                          Option("t", "", "Using threads."),
		                                          Option("c", "", "Communication option: 0 no comm 1 for \"place 0\", 2 for all-to-all and 3 for neighbors"),
		                                          Option("i", "", "Communication Interval (iterations) . Default 10."),
		                                          Option("g", "", "Number of groups. Default 4."),
		                                          Option("k", "", "poolsize.")
		                                          ]);
		
		val cspProblem = opts("-p", "magic-square");
		val size = opts("-s", 10);
		val testNo = opts("-b", 10);
		val solverMode = opts("-m", 0);
		val threads = opts("-t", 0);
		val comm = opts("-c", 0);
		val inter = opts("-i", 10000000);
		val groupN = opts("-g", 4);
		val poolSize = opts("-k", 4);
		
		

		Console.OUT.println("CSP Problem: "+cspProblem+" Size: "+size+"\nNumber of repetitions: "+testNo+
				" SolverMode: "+solverMode+" Communication strategy: "+comm+" Communication update inteval: "+inter+
				"\nThreads:"+threads+" Pool Size: "+poolSize);
		
		var param:Int = 0;
		//var file : String = "";
				
		if (cspProblem.equals("magic-square")) {
			Console.OUT.println("Magic Square Problem");
			param = 1;
		}else if(cspProblem.equals("costas")){
			Console.OUT.println("Costas Array Problem");
			param = 2;
		}else if(cspProblem.equals("all-interval")){
			Console.OUT.println("All-Interval Array Problem");
			param = 3;
		}else if(cspProblem.equals("langford")){
			Console.OUT.println("Langford Pairing Problem");
			param = 4;
		}else if(cspProblem.equals("partit")){
			Console.OUT.println("Number Partition Problem");
			param = 5;
		}
		// else if(cspProblem.equals("QAP")){
		// 	Console.OUT.println("Quadratic assignment Problem");
		// 	param = 99;
		// 	file = argv(4);
		// }
		else{
			Console.OUT.println("Error: Type a valid CSP example: magic-square or costas"); 
			return;
		}
		
		/*
		 *  Creating objects for solver execution
		 */
		
		var timeStart:Long;
		var cost:Int;
		var timeEnd :Long;
		var sumTimes:Long = 0;
		val accStats = new CSPStats();
		
		// communication interval = 10
		val solverP = new ASSolverPermutRW(inter, comm, threads, poolSize, groupN); //this line -----***-----
		val solverT = new ASSolverPermutRWActivities(inter,solverMode);

		if (solverMode == 0){
			Console.OUT.println("Using "+Place.MAX_PLACES+" Places");
		} else{
			Console.OUT.println("Using "+solverMode+" Activities");
		}
		
		Console.OUT.println("|Count| Time (s) |  Iters   |Place|  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| Change|");
		Console.OUT.println("|-----|----------|----------|-----|----------|----------|----------|-------|-----|-------|");
		
		/*
		 *  Execution loop
		 */
		for (var j : Int = 1; j <= testNo ; j++ ){
			
			//Solve the problem
			val stats:CSPStats;
			if (solverMode == 0){ 
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
		Console.OUT.println("|-----|----------|----------|-----|----------|----------|----------|-------|-----|-------|");
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