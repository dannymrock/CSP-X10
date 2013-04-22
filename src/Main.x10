/** Main
 * 	Main class of the project. 
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013 	-> First Version
 * 					12 April, 2013	-> TLP support
 */

import x10.util.Random;
public class Main {
	public static def main(argv:Array[String]):void {
		/****************** RW *****************************/
		
		var size1:Int;
		var size : Int;
		val testNo : Int; 
		val cspProblem : String;
		
		val argc = argv.size; 
		
		cspProblem = argv(0);
		size = Int.parse(argv(1));
		
		testNo = Int.parse(argv(2));
		val updateI = Int.parse(argv(3));
		
		var param:Int = 0;
				
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
		}else{
			Console.OUT.println("Error: Type a valid CSP example: magic-square or costas");
			return;
		}
		 
		var timeStart:Long;
		var cost:Int;
		var timeEnd :Long;
		var sumTimes:Long = 0;
		val accStats = new CSPStats();
		
		val solverP = new ASSolverPermutRW(updateI); //this line -----***-----
		val solverT = new ASSolverPermutRWActivities(updateI,updateI);
		
		
		if (updateI == 0){
			Console.OUT.println("Using "+Place.MAX_PLACES+" Places");
		} else{
			Console.OUT.println("Using "+updateI+" Activities");
		}
		
		Console.OUT.println("|Count| Time (s) |  Iters   |Place|  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta|");
		Console.OUT.println("|-----|----------|----------|-----|----------|----------|----------|-------|-----|");
		
		for (var j : Int = 1; j <= testNo ; j++ ){
			
			//Solve the problem
			val stats:CSPStats;
			if (updateI == 0){ 
				stats = solverP.solve(size,param);
			}else{
				stats = solverT.solve(size,param);
			}
			accStats.accStats(stats);
			Console.OUT.printf("\r");
			stats.print(j);
			accStats.printAVG(j);
			Console.OUT.flush();
		}
		Console.OUT.printf("\r");
		Console.OUT.println("|-----|----------|----------|-----|----------|----------|----------|-------|-----|");
		accStats.printAVG(testNo);
		Console.OUT.printf("\n");
		/**********************************/
		
		/*val cspProblem:String;
		var size:Int; 
		val parallel:Int;
		val testNo:Int;
		val r = new Random();
		
		val argc = argv.size; 
		if (argc < 3){
			Console.ERR.println("USAGE: ./Main <CSPProblem> <SizeP> <#Parallel Implementation = 0, 1, 2 o 3> <No. tests>");
			return; 
		}
		cspProblem = argv(0);
		size = Int.parse(argv(1));
		parallel = Int.parse(argv(2));
		testNo = Int.parse(argv(3));
		
		
		//Select Random Seed
		val seed:Long = r.nextLong();
		Console.OUT.println("Random Seed: "+seed);
		val rt : RandomTools = new RandomTools(seed);
		
		
		//Create CSP Object
		val cspObject:ModelAS;
		if (cspProblem.equals("magic-square")) {
			Console.OUT.print("Magic Square Problem with ");
			cspObject = new MagicSquareAS(size, seed);
			size = size * size;
		}else if(cspProblem.equals("queens")){
			Console.OUT.print("N-Queens Problem with ");
			cspObject = new QueensAS(size, seed);
		}else{
			Console.OUT.println("Error: Type a valid CSP example: magic-square or queens");
			return;
		}
		
		//Create CSP Solver
		val solver:ASSolverPermut;
		if (parallel == 0) {
			Console.OUT.println("Sequential Solver.");
			solver = new ASSolverPermut(size, seed);
		}else if(parallel == 1){
			Console.OUT.println("Parallel Solver (1st Approach).");
			solver = new ASSolverPermutTLP(size,2,seed);
		}else if(parallel == 2){
			
			Console.OUT.println("Parallel Solver (2st Approach).");
			solver = new ASSolverPermut(size, seed);
		}else if(parallel == 3){
			Console.OUT.println("Parallel Solver (3st Approach).");
			solver = new ASSolverPermut(size, seed);
		}
		else{
			Console.OUT.println("Error: Choose valid solver: 0 for seq, 1 for first parallel app, 2 for second parallel app");
			return;
		}
		
		// Solving Problem
		var timeStart:Long;
		var cost:Int;
		var timeEnd :Long;
		var sumTimes:Long = 0;
		for (var j:Int = 0; j < testNo ; j++ ){
			timeStart = x10.lang.System.currentTimeMillis();
			cost = solver.solve(cspObject);
			timeEnd = x10.lang.System.currentTimeMillis();
			//if (testNo == 1)
				//show("Solution = ", cspObject.getVariables());
			Console.OUT.println("\tTime= "+(timeEnd-timeStart)+" ms");
			sumTimes += (timeEnd-timeStart);
			solver.clear();
		}
		Console.OUT.println("Time AVG= "+(sumTimes/testNo)+" ms");
		*/
		
		
		/****** costas ******/
		
		/*val csp = new AllIntervalAS(10, 2);
		csp.initialize(0);
		show("sol", csp.variables);
		val solver = new ASSolverPermut(csp.length, 2, 1);
		var cost :Int = csp.costOfSolution(1); 
		Console.OUT.println("cost= "+cost);
		var max_i :Int = 6;
		Console.OUT.println("max ="+max_i);
		for (i in 0..9){
			var cost1 :Int = csp.costIfSwap(cost,i,max_i);
			Console.OUT.println("swap "+i+"/"+max_i+" = "+cost1);
		}
		csp.swapVariables(3,6);
		show("sol swap", csp.variables);
		csp.executedSwap(3,6);
		cost = csp.cost();
		Console.OUT.println("new cost= "+cost);
		max_i = solver.selectVarHighCost(csp);
		Console.OUT.println("new max ="+max_i);
		//val min = solver.selectVarMinConflict(csp);
		// show("sol", cspObject.variables);
		
		//cspObject.initialize(1);
		//show("sol", cspObject.variables);
		//val cost = cspObject.costOfSolution(1);
		//Console.OUT.println("cost= "+cost);
		*/
		/******************/

		return;
	}

	static def show(s:String, d: Array[Int]) {
		x10.io.Console.OUT.print(s + " = ");
		//finish for (p in d.dist.places()) at(p) async{
			for(k in d) 
				x10.io.Console.OUT.print(" " + d(k));
		//}
		
		x10.io.Console.OUT.println("");
	}
}