/** Main
 * 	Main class of the project. 
 * 	@author Danny Munera
 */

package csp.model;
import csp.solver.ParallelSolverI;
import csp.solver.PlacesMultiWalks;
import csp.util.Logger;
import x10.util.Team;
import x10.util.OptionsParser;
import x10.util.Option;
import x10.util.Random;
import x10.compiler.Pragma;
import x10.io.File;
import x10.io.FileReader;
import x10.io.FileWriter;
import x10.util.StringBuilder;

public class Main {
	 
	 public static struct CSPProblem(kind:Int) 
	 {
		  public def make( size : Long, vectorSize : Long, seed : Long, 
					          mPrefs : Rail[Rail[Int]], wPrefs : Rail[Rail[Int]], 
					         restLimit : Int, mapTable : Rail[Int], inVector : String ) 
		            : ModelAS(vectorSize) 
		  {
				if (kind == MAGIC_SQUARE_PROBLEM) 
					 return new MagicSquareAS(size as Int, vectorSize, seed, restLimit, inVector);
				if (kind == COSTAS_PROBLEM) 
					 return new CostasAS(vectorSize, seed, restLimit, inVector);
				if (kind == ALL_INTERVAL_PROBLEM) 
					 return new AllIntervalAS(vectorSize, seed, true, restLimit, inVector);
				if (kind == LANGFORD_PROBLEM) 
					 return new LangfordAS(size, vectorSize, seed, restLimit, inVector);
				if (kind == STABLE_MARRIAGE_PROBLEM) 
					 return new SMTIAS(vectorSize, seed, mPrefs, wPrefs, restLimit, mapTable, false, inVector);
				if (kind == HOSPITAL_RESIDENT_PROBLEM) 
					 return new SMTIAS(vectorSize, seed, mPrefs, wPrefs, restLimit, mapTable, true, inVector);
				if (kind == QA_PROBLEM) 
					 return new QAPAS(vectorSize, seed, mPrefs, wPrefs, restLimit, inVector);
				return new PartitAS(vectorSize, seed, restLimit, inVector);
			}
	 }
	
	public static val UNKNOWN_PROBLEM=0n;
	public static val MAGIC_SQUARE_PROBLEM = 1n;
	public static val COSTAS_PROBLEM = 2n;
	public static val ALL_INTERVAL_PROBLEM = 3n;
	public static val LANGFORD_PROBLEM = 4n;
	public static val PARTIT_PROBLEM = 5n;
	public static val STABLE_MARRIAGE_PROBLEM = 6n;
	public static val HOSPITAL_RESIDENT_PROBLEM = 7n;
	public static val QA_PROBLEM = 8n;

	
	var fp  : File;
	
	public static def main(args:Rail[String]):void{
		
		var totalTime:Long = -System.nanoTime();
		
		/**
		 *   Parsing input
		 */
		val opts = new OptionsParser(args, 
				new Rail[Option](0L), [
				                       Option("p", "", "(p)roblem to solve"),
				                       Option("f", "", "(f)ile path for SMTI or QAP"),
				                       Option("s", "", "(S)ize of the problem"),
				                       Option("m", "", "Solver (m)ode distribution 0 for Places \"n\" for Activities (n number of activities). Default 0."),
				                       Option("l", "", "restart (l)imit"),
				                       Option("t", "", "(T)ime out default 0"),
				                       Option("c", "", "target (c)ost from Command Line Parameter. default 0"),
				                       Option("a", "", "Flag to receive target cost form file. default 0 from command line, 1 take optimal from file, 2 take BKS from file "),
				                       Option("b", "", "Number of (b)enchmark tests"),
				                       Option("N", "", "nodes_per_team parameter. Default 4."),
				                       Option("U", "", "Update Interval Intra-team Communication (iterations) . Default 0 - no communication."),
				                       Option("R", "", "Report Interval Intra-team Communication (iterations) . Default 0 - no communication."),
				                       Option("C", "", "Probability to change vector in Intra-Team Communication "),
				                       Option("P", "", "poolsize."),
				                       Option("I", "", "Inter-team Communication Interval (miliseconds) . Default 0 - no communication."),
				                       Option("D", "", "minimum permisible distance."),
				                       Option("W", "", "initial (W)ait  before start Inter-team Communication (miliseconds). Default 0"),
				                       Option("A", "", "Inter Team Communicaction Diversification - Percentage of Places (A)ffected . Default 0."),
				                       Option("y", "", "seed. Default 0"),
				                       Option("v", "", "verify and print solution. Default 0"),
				                       Option("i", "", "file path for input vector . Default ."),
				                       Option("o", "", "output format: csv 0, info 1")
				                       ]);
		
		val problem        = opts("-p", "MSP");
		val filePath       = opts("-f", ".");
		val size           = opts("-s", 10n);
		val solverMode	    = opts("-m", 0n);
		val restartLimit   = opts("-l", 1000000000n);
		val maxTime        = opts("-t", 0);
		val tCostFromCL    = opts("-c", 0n);
		val costFromF      = opts("-a", 0);
		val testNb         = opts("-b", 10n);
		val nodesPTeam     = opts("-N", 1n);		
		val updateI        = opts("-U", 0n);
		val reportI        = opts("-R", 0n);
		val changeProb     = opts("-C", 100n);
		val poolSize       = opts("-P", 4n);
		val interTI        = opts("-I", 0);
		val minDistance    = opts("-D", 0.3);
		val delayI         = opts("-W", 0);
		val affectedP      = opts("-A", 0.0);
		val inSeed         = opts("-y", 0);
		val verify         = opts("-v", 0);
		val inputPath      = opts("-i", ".");
		val outFormat	    = opts("-o", 1n);
			

		
		/**
		 *   Print Parameters
		 */
		Console.OUT.println("Problem "+problem+" size "+size+" File Path (SMTI):"+filePath); 
		Console.OUT.println("Solver: Mode "+(solverMode==0n ?"sequential":"parallel")+", Limit: "+restartLimit+ " iterations or "+maxTime+" ms.");
		Console.OUT.println("Target cost from "+(costFromF  == 0  ? "command line. " :
			                                     ((tCostFromCL >= 0n ? "file, lower or equal than ":
			                                    	"file, strictly lower than ")+ Math.abs(tCostFromCL))));
		Console.OUT.println("Solving "+testNb+" times each instance");
		Console.OUT.println((nodesPTeam > 1n ? "Using ":"Without ")+"Cooperative Search: "+Place.MAX_PLACES+" places. "+nodesPTeam+" nodes per team "+(Place.MAX_PLACES as Int / nodesPTeam)+" Teams");
		Console.OUT.println("Intensification Parameters: Update Interval "+updateI+" iter. Report Interval "+reportI+" iter. Pool size "+poolSize+" conf. Probability to Change vector "+changeProb+"%");
		Console.OUT.println("Diversification Parameters: Interval "+interTI+" ms. Minimum distance: "+minDistance+" Initial delay "+delayI+" ms. Per. Affected Places "+(affectedP*100)+"%");
		Console.OUT.println("Input seed "+inSeed+ " Input vector "+(inputPath.equals(".")?"not used":inputPath));
		Console.OUT.println("Max threads "+Runtime.MAX_THREADS+" NTHREADS "+ Runtime.NTHREADS );
		
		/**
		 *   Define basic values for each type of problem
		 */
		var param : Int = UNKNOWN_PROBLEM;
		
		var vectorSize:Long = size; //?
		if (problem.equalsIgnoreCase("MSP"))
		{
			 //Logger.debug(()=>{"Magic Square Problem"});
			 param = MAGIC_SQUARE_PROBLEM;
			 vectorSize = size*size;
		} 
		else if(problem.equals("CAP"))
		{
			 //Logger.debug(()=>{"Costas Array Problem"});
			 param = COSTAS_PROBLEM;
			 vectorSize = size;
		}
		else if(problem.equals("AIP"))
		{
			 //Logger.debug(()=>{"All-Interval Array Problem"});
			 param = ALL_INTERVAL_PROBLEM;
			 vectorSize = size;
		}
		else if(problem.equals("LNP"))
		{
			 //Logger.debug(()=>{"Langford Pairing Problem"});
			 param = LANGFORD_PROBLEM;
			 val eNumber = 3n; //entanglement number 2 for pairs, 3 for triplets. Parameter K
			 vectorSize = eNumber*size;
		}
		else if(problem.equals("NPP"))
		{
			 //Logger.debug(()=>{"Number Partition Problem"});
			 param = PARTIT_PROBLEM;
			 vectorSize = size;
		}
		else if(problem.equals("SMP"))
		{
			 //Logger.debug(()=>{"Stable Marriage Problem"});
			 param = STABLE_MARRIAGE_PROBLEM;
			 vectorSize = size;
		}
		else if(problem.equals("HRP"))
		{
			 //Logger.debug(()=>{"Stable Marriage Problem"});
			 param = HOSPITAL_RESIDENT_PROBLEM;
			 vectorSize = size;
		}
		else if(problem.equals("QAP"))
		{
			 //Logger.debug(()=>{"Stable Marriage Problem"});
			 param = QA_PROBLEM;
			 vectorSize = size;
		}
		else
		{
			 Console.OUT.println("Error: Type a valid CSP example: MSP, CAP, AIP, LNP, NPP , SMP, HRP or QAP"); 
			 return;
		}
		
		/**
		 *  Creating objects for solver execution
		 */
		val seed = inSeed;//(inSeed == 0) ? j as Long:inSeed;
		val random = new Random(seed);	
		
		val fileMode = (param == STABLE_MARRIAGE_PROBLEM || param == HOSPITAL_RESIDENT_PROBLEM 
		|| param == QA_PROBLEM );
		
		val vectorSz = vectorSize;
		val solvers:PlaceLocalHandle[ParallelSolverI(vectorSz)];
		solvers = PlaceLocalHandle.make[ParallelSolverI(vectorSz)](PlaceGroup.WORLD, 
				()=>new PlacesMultiWalks(vectorSz, updateI, reportI, interTI, poolSize, nodesPTeam,
						changeProb, minDistance, maxTime, (verify!=0), delayI, 
						affectedP) as ParallelSolverI(vectorSz));
		 
		var insNb:Int = 0n; //counter of instances
		var iList : Rail[String];
		
		// accumulated times
		var totalWallT :Long = 0;

		/**
		 *  Install solver data structures on every available place
		 */
		finish for (p in Place.places()) at (p) async{    
			solvers().installSolver(solvers);
		}
		
		val nPath = new StringBuilder();
		if ( fileMode ){
			//Load Files enable double loop
			iList = SMTIAS.loadDir(filePath,nPath);
			//Logger.info(()=>{"nPath "+nPath});
		}else{
			//disable double loop
			iList = ["noFile" as String];
		}
		
		
		/**
		 *  1st Loop: for all files in the list (used for SMTI an QAP) 
		 */
		for (instance in iList)
		{
			 //I expect to receive 4 parameters
			 val problemParams = new Rail[Long](4, -1 );

			 // mode equal to:
			 // 1 -> filemode==true && sucess (valid file)
			 // 2 -> filemode==true && !sucess (not valid file eg. directory)   
			 // 3 -> filemode==false
			 val mode = (fileMode) ? (SMTIAS.tryReadParameters( nPath+"/"+instance, problemParams)? 1 : 2 ): 3;
			 
			 // if filemode == true and "path" is a directory -> skip
			 if ( mode == 2 ) continue; 
			 
			 /** First parameter (in the file) is the size of the problem 
			  *  in SMP:   n1 -> number of men         n2 -> number of women
			  *  in HRP:   n1 -> number of residents   n2 -> number of hospitals
			  *  in QAP:   n1 -> number of facilities  n2 -> number of locations (n1 == n2)
			  *  in noFile: n1 = n2 = 1  - matrix1, matrix2 and mapTable are not used
			  */
			 val n1 = problemParams(0) < 0 ? 1 : problemParams(0);
			 
			 val n2 = (mode == 3 || problemParams(1) < 0) ? 1 :     // default value
					    param == QA_PROBLEM                 ? n1:     // n1 == n2  
					    problemParams(1);                             // file loaded value 
			
			 /**
			  * Data structures for file problems SMP HRP QAP
			  * if fileMode is false create "empty" matrices of size 1
			  */
			 // Warning: matrices 1 and 2 must be always square matrices, because of the 
			 // cloning technique implementation in HRT problems 
			 val matrix1 = new Rail[Rail[Int]](n1, (Long) => new Rail[Int]( n1, 0n ));
			 val matrix2 = new Rail[Rail[Int]](n1, (Long) => new Rail[Int]( n1, 0n ));
			 // maping table for HRT problems "cloning"
			 val mapTable = new Rail[Int]( n2, ( i : long ) => i as Int );
			 
			 var opt : Long = 0;
			 var bks : Long = 0;
			 
			 var loadTime:Long = -System.nanoTime();
			 if ( param == STABLE_MARRIAGE_PROBLEM)
			 {	
				  // load men and women preferences for the SMTI problem or
				  SMTIAS.loadData( nPath+"/"+instance, n1, n2, matrix1, matrix2);
				  opt = problemParams(2);
				  bks = problemParams(3);
			 } else if (param == HOSPITAL_RESIDENT_PROBLEM){
				  // load residents hospitals preference lists for HRTP - size is n1
				  SMTIAS.loadData(nPath+"/"+instance, n1, n2, matrix1, matrix2, mapTable);
				  opt = problemParams(2);
				  bks = problemParams(3);
			 } else if (param == QA_PROBLEM){
				 // load flow and distance matrices for QAP 
				 SMTIAS.loadData(nPath+"/"+instance, n1, n2, matrix1, matrix2);
				 opt = problemParams(1);
				 bks = problemParams(2);
			}
			val cT= loadTime += System.nanoTime();
			//Logger.debug(()=>{"Time to load the file problem="+cT/1e9});
			
			
			
			var c : Long = 0;
			var sl : Boolean = false;
			
			if ( costFromF == 0 ) // target cost loaded from command line parameter
			{	
				 if (tCostFromCL >= 0) // get lower or equal to target 
				 {	
					  c = tCostFromCL;
					  sl = false;
					  //ff = false;
				 } else 
				 {	
					  c = tCostFromCL * -1;
					  sl = true;
				 }
			} else // target cost loaded from file
			{
				 sl = costFromF < 0; // strictly lower true for negative numbers
				 if ( costFromF == 1  || costFromF == -1  ) // try to get optimal cost
					  c = opt; 
				 else
					  c = bks;
			}
			
			val tCost = c >= 0? c : 0; // if negative cost put default value
			val sLow = sl;
			
			insNb++;
			printHeader(outFormat);
			
			/**
			 *  2nd Loop: for repetition (testNb) 
			 */
			for (var j:Int = 1n ; j<=testNb; j++)
			{
				var wallTime:Long = -System.nanoTime();	
				val modelSeed = random.nextLong();
				val prob = param;
				val cspGen = ():ModelAS(vectorSz)=>CSPProblem(prob).make(size as Long,vectorSz,
						modelSeed,matrix1, matrix2,restartLimit, mapTable, inputPath);
				
				/**
				 *   Start remote solver processes
				 */
				if (solverMode == 0n){
					finish for (p in Place.places()) 
					{
						val solverSeed = random.nextLong(); 
						at (p) async
						solvers().solve(solvers, cspGen, solverSeed, tCost, sLow);   
					}
				}else{
					finish for(var i:Long = Place.MAX_PLACES-1; i >= 0; i-= 16) at (Place(i)) async 
					{
						val max = here.id; val min = Math.max(max-15, 0);
						val r = new Random(random.nextLong()+here.id);
						finish for(k in min..max)
						{
							val solverSeed = r.nextLong();
							at(Place(k)) async  solvers().solve(solvers, cspGen, solverSeed, tCost, sLow );
						}
					}
				}
				
				/**
				 *   Report best result when there is no winner
				 */
				solvers().verifyWinner(solvers);
				
				wallTime += System.nanoTime();
				val wtime = wallTime;
				totalWallT += wallTime;
				//Logger.debug(()=>{"wall time="+wtime/1e9});
				
				/**
				 *  Print stats for the current instance and sample
				 */
				if(outFormat == 0n){
					if (fileMode) Console.OUT.print(instance+",");
					solvers().printStats(j,outFormat);
					Console.OUT.println(","+wallTime/1e9);
				}
				else if (outFormat == 1n){
					Console.OUT.printf("\r");
					solvers().printStats(j,outFormat);
					Console.OUT.printf(" %3.2f \n",wallTime/1e9);
					solvers().printAVG(j,outFormat);
					Console.OUT.flush();
				}
				
				/**
				 *  Clear solvers data structures 
				 *  (to start the soving process with other instance)
				 */
				finish for (p in Place.places()) at (p) async{   
					solvers().clear();
				}	
				
			} // End 2nd Loop
		
			/**
			 *   Print average of repetitions  
			 */
			if(outFormat == 0n){
				//Console.OUT.print(file+",");
				solvers().printAVG(testNb,outFormat);
			}
			else if (outFormat == 1n){
				Console.OUT.printf("\r");
				Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|---------|-----|");
				Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  | frP-frT |  PS |");
				Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|---------|-----|");
				solvers().printAVG(testNb,outFormat);
				//accStats.printAVG(testNo);
				Console.OUT.printf("\n");
			}
			// Clear sample accumulator for repetitions
			solvers().clearSample();
			
			for(mline in matrix1) mline.clear();
			for(mline in matrix2) mline.clear();
		}//End 1st Loop
		
		/**
		 *  Print General average if necessary (SMTI QAP)
		 */
		if(outFormat == 0n){
			Console.OUT.print("TOTAL,");
			solvers().printGenAVG(insNb*testNb,outFormat);
		}else if (outFormat == 1n){
			Console.OUT.println("|------------------------------------------------------------------------------------------------------------------------|");
			Console.OUT.println("\n   General Statistics for "+insNb+" problems, each one solved "+testNb+" times ");
			Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|---------|-----|");
			Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  | frP-frT |  PS |");
			Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|---------|-----|");
			solvers().printGenAVG(insNb*testNb,outFormat);
			//accStats.printAVG(testNo);
			Console.OUT.printf("\n");
		}
		
		val avgWall = totalWallT/(insNb*testNb as Double);
		totalTime += System.nanoTime();
		if(outFormat == 0n)
			Console.OUT.println("TotalInstances,"+insNb+",,Repetitions,"+testNb+",,Total_Time,"+(totalTime/1e9)+
					",,AVG_wall_Time,"+(avgWall/1e9));
		else if (outFormat == 1n)
			Console.OUT.println("TotalInstances: "+insNb+" Repetitions: "+testNb+" Total_Time: "+(totalTime/1e9)+
					" AVG_wall_Time: "+(avgWall/1e9));	
		
		return;
	}	
	
	static def printHeader(outF : Int){
		if(outF == 0n){
			Console.OUT.println("instance,count,time(s),iters,place,local_Min,swaps,resets,same/iter,restarts,blocking_pairs,singles,Changes,fRestP,fRestT,solution,walltime");
		}else if(outF == 1n){
			Console.OUT.println("|------------------------------------------------------------------------------------------------------------------------|");
			Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  | frP-frT |  PS | walltime");
			Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|---------|-----|");
		}
	}
}
