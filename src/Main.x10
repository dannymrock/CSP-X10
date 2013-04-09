import x10.util.Random;

public class Main {
	
	public static def main(argv:Array[String]):void {
		
		/****************** RW *****************************/
		
		var size1:Int;
		var size:Int;
		val testNo:Int; 
		val cspProblem:String;
		
		val argc = argv.size; 
		
		cspProblem = argv(0);
		size = Int.parse(argv(1));
		
		testNo = Int.parse(argv(2));
		val update = Int.parse(argv(3));
		var param:Int = 0;
				
		if (cspProblem.equals("magic-square")) {
			Console.OUT.println("Magic Square Problem");
			param=1;
		}else if(cspProblem.equals("queens")){
			Console.OUT.println("N-Queens Problem");
			param=2;
		}else{
			Console.OUT.println("Error: Type a valid CSP example: magic-square or queens");
			return;
		}
		 
		var timeStart:Long;
		var cost:Int;
		var timeEnd :Long;
		var sumTimes:Long = 0;
		
		val solver = new ASSolverPermutRW(update);
		
		for (var j:Int = 0; j < testNo ; j++ ){
			timeStart = x10.lang.System.currentTimeMillis();
			cost = solver.solve(size,param);
			timeEnd = x10.lang.System.currentTimeMillis();
			Console.OUT.println("\tTime= "+(timeEnd-timeStart)+" ms"+" cost= "+cost);
			sumTimes += (timeEnd-timeStart);
		}
		Console.OUT.println("Time AVG= "+(sumTimes/testNo)+" ms");
		
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
		
		
		/****** All Interval *****
		
		val cspObject = new AllIntervalAS(10, 2);
		val solver = new ASSolverPermut(cspObject.length, 2);
		val cost = solver.solve(cspObject);
		Console.OUT.println("cost= "+cost);
		
		show("sol", cspObject.variables);
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