/** Main
 * 	Main class of the project. 
 * 	@author Danny Munera
 */

package csp.model;
import csp.solver.IParallelSolver;
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
import csp.solver.RandomSearch;
import csp.solver.EOSearch;
import csp.solver.AdaptiveSearch;

public class Main {
	 
	 public static struct CSPProblem(kind:Int) 
	 {
		  public def make( size : Long, vectorSize : Long, seed : Long, opts:ParamManager, 
					 mPrefs : Rail[Rail[Int]], wPrefs : Rail[Rail[Int]], mapTable : Rail[Int]) 
		  : ModelAS(vectorSize) 
		  {
				if (kind == MAGIC_SQUARE_PROBLEM) 
					 return new MagicSquareAS(size, vectorSize, seed, opts);
				if (kind == COSTAS_PROBLEM) 
					 return new CostasAS(vectorSize, seed, opts);
				if (kind == ALL_INTERVAL_PROBLEM) 
					 return new AllIntervalAS(vectorSize, seed, opts);
				if (kind == LANGFORD_PROBLEM) 
					 return new LangfordAS(size, vectorSize, seed, opts);
				if (kind == STABLE_MARRIAGE_PROBLEM) 
					 return new SMTIAS(vectorSize, seed, opts, false, mPrefs, wPrefs, mapTable);
				if (kind == HOSPITAL_RESIDENT_PROBLEM) 
					 return new SMTIAS(vectorSize, seed, opts, true, mPrefs, wPrefs, mapTable);
				if (kind == QA_PROBLEM) 
					 return new QAPAS(vectorSize, seed, opts, mPrefs, wPrefs );
				return new PartitAS(vectorSize, seed, opts);
		  }
	 }
	 
	 public static struct Solver(kind:Int) 
	 {
		  public def make( size : Long, nsize : Int, ss : IParallelSolver(size),
					 opts:ParamManager) 
		  : RandomSearch(size) 
		  {
				if (kind == AS_SOL) 
					 return new AdaptiveSearch( size, nsize, ss , opts) ;
				if (kind == EO_SOL) 
					 return new EOSearch( size, nsize, ss, opts);
				return new RandomSearch( size, nsize, opts);
		  }
	 } 
	 
	 // Problems
	 public static val UNKNOWN_PROBLEM=0n;
	 public static val MAGIC_SQUARE_PROBLEM = 1n;
	 public static val COSTAS_PROBLEM = 2n;
	 public static val ALL_INTERVAL_PROBLEM = 3n;
	 public static val LANGFORD_PROBLEM = 4n;
	 public static val PARTIT_PROBLEM = 5n;
	 public static val STABLE_MARRIAGE_PROBLEM = 6n;
	 public static val HOSPITAL_RESIDENT_PROBLEM = 7n;
	 public static val QA_PROBLEM = 8n;
	 // Solvers
	 public static val UNKNOWN_SOL = 0n;
	 public static val AS_SOL = 1n;
	 public static val EO_SOL = 2n;
	
	 var fp  : File;
	 
	 public static def main(args:Rail[String]):void{
		  
		  var totalTime:Long = -System.nanoTime();
		  
		  /**
		   *   Parsing input
		   */
		  val opts = new ParamManager(args);
	     val help = opts("-h");
		  opts.parseFile();
		  
		  if (help){
				Console.OUT.println(opts.usage("CPLS Solver v 0.1 \n"));
				return;
		  }
		  // print all the parameters of the Program
		  opts.printParameters();
		  
		  
		  /**
		   *  Param used in Main
		   */
		  val size           = opts("-s", 10n);
		  val problem        = opts("-p", "MSP");
		  val filePath       = opts("-f", ".");
		  val solverMode	   = opts("-sm", 0n);
		  val solverIn       = opts("-sl", "AS");
		  val inSeed         = opts("-S", 0);
		  val outFormat	   = opts("-of", 1n);
		  val costFromF      = opts("-tf", 0);
		  val tCostFromCL    = opts("-tc", 0n);
		  val testNb         = opts("-b", 10n);
		  
		  /**
		   *   Define basic values for each type of problem
		   */
		  
		  var problemParam:Int = UNKNOWN_PROBLEM;
		  var vectorSize:Long = size; //?
		  if (problem.equalsIgnoreCase("MSP"))
		  {
				//Logger.debug(()=>{"Magic Square Problem"});
				problemParam = MAGIC_SQUARE_PROBLEM;
				vectorSize = size*size;
		  } 
		  else if(problem.equals("CAP"))
		  {
				//Logger.debug(()=>{"Costas Array Problem"});
				problemParam = COSTAS_PROBLEM;
				vectorSize = size;
		  }
		  else if(problem.equals("AIP"))
		  {
				//Logger.debug(()=>{"All-Interval Array Problem"});
				problemParam = ALL_INTERVAL_PROBLEM;
				vectorSize = size;
		  }
		  else if(problem.equals("LNP"))
		  {
				//Logger.debug(()=>{"Langford Pairing Problem"});
				problemParam = LANGFORD_PROBLEM;
				val eNumber = 3n; //entanglement number 2 for pairs, 3 for triplets. Parameter K
				vectorSize = eNumber*size;
		  }
		  else if(problem.equals("NPP"))
		  {
				//Logger.debug(()=>{"Number Partition Problem"});
				problemParam = PARTIT_PROBLEM;
				vectorSize = size;
		  }
		  else if(problem.equals("SMP"))
		  {
				//Logger.debug(()=>{"Stable Marriage Problem"});
				problemParam = STABLE_MARRIAGE_PROBLEM;
				vectorSize = size;
		  }
		  else if(problem.equals("HRP"))
		  {
				//Logger.debug(()=>{"Stable Marriage Problem"});
				problemParam = HOSPITAL_RESIDENT_PROBLEM;
				vectorSize = size;
		  }
		  else if(problem.equals("QAP"))
		  {
				//Logger.debug(()=>{"Stable Marriage Problem"});
				problemParam = QA_PROBLEM;
				vectorSize = size;
		  }
		  else
		  {
				Console.OUT.println("Error: Type a valid CSP example: MSP, CAP, AIP, LNP, NPP , SMP, HRP or QAP"); 
				return;
		  }
		  
		  /**
		   *   Define basic values for each type of Solver
		   */
		  var solParam : Int = UNKNOWN_SOL;
		  
		  if (solverIn.equalsIgnoreCase("AS"))
				solParam = AS_SOL;
		  else if(solverIn.equals("EO"))
				solParam = EO_SOL;
		  
		  
		  
		  /**
		   *  Creating objects for solver execution
		   */
		  val seed = inSeed;//(inSeed == 0) ? j as Long:inSeed;
		  val random = new Random(seed);	
		  
		  val fileMode = (problemParam == STABLE_MARRIAGE_PROBLEM || problemParam == HOSPITAL_RESIDENT_PROBLEM 
					 || problemParam == QA_PROBLEM );
		  
		  val vectorSz = vectorSize;
		  
		  val solvers:PlaceLocalHandle[IParallelSolver(vectorSz)];
		  
		  solvers = PlaceLocalHandle.make[IParallelSolver(vectorSz)](PlaceGroup.WORLD, 
					 ()=>new PlacesMultiWalks(vectorSz, opts) as IParallelSolver(vectorSz));
		  
		  var insNb:Int = 0n; //counter of instances
		  var iList : Rail[String];
		  
		  // accumulated times
		  var totalWallT :Long = 0;
		  
		  
		  val sparam = solParam;
		  //val ss = solvers() as IParallelSolver(vectorSz);
		  //val solGen = ():ISolver(vectorSz)=>Solver(sparam).make( vectorSz, size, solvers() as IParallelSolver(vectorSz), maxTime );
		  val solGen = ():RandomSearch(vectorSz)=>Solver(sparam).make( vectorSz, size, 
					 solvers() as IParallelSolver(vectorSz), opts );
		  
		  /**
		   *  Install solver data structures on every available place
		   */
		  finish for (p in Place.places()) at (p) async{    
				solvers().installSolver(solvers, solGen);
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
		  
		  
		  var cFile : String = new String (""); 
		  var fWall : Long = 0;
		  if(outFormat == 0n){
				Console.OUT.println("1_Instance,2_Count,3_Time(s),4_Iters,*5_Place,6_Local_Min,7_Swaps,8_Resets,"
						  +"9_Same/iter,10_Restarts,11_Blocking_Pairs,12_Singles,13_Changes,14_Force_Restart_Place,15_Force_Restart_Team,"
						  +"16_Perfect_Sol,17_Target_Ac,18_Cost,*19_Distance_to_Target,20_Gap,21_Wall_Time");
		  }
		  
		  /**
		   *  1st Loop: for all files in the list (used for SMTI an QAP) 
		   */
		  for (instance in iList)
		  {
				fWall = 0;
				cFile = instance;
				//I expect to receive 4 parameters
				val problemParams = new Rail[Long](4, -1 );
				
				// mode equal to:
				// 1 -> filemode==true && sucess (vali.0/bcgilmptwd file)
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
				
				val n2 = (mode == 3 || problemParams(1) == -1) ? 1 :     // default value
					 problemParam == QA_PROBLEM ? n1:     // n1 == n2  
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
				val mapTable = new Rail[Int]( n1, ( i : long ) => i as Int );
				
				var opt : Long = 0;
				var bks : Long = 0;
				
				var loadTime:Long = -System.nanoTime();
				if ( problemParam == STABLE_MARRIAGE_PROBLEM)
				{	
					 // load men and women preferences for the SMTI problem or
					 SMTIAS.loadData( nPath+"/"+instance, n1, n2, matrix1, matrix2);
					 opt = problemParams(2);
					 bks = problemParams(3);
				} else if (problemParam == HOSPITAL_RESIDENT_PROBLEM){
					 // load residents hospitals preference lists for HRTP - size is n1
					 SMTIAS.loadData(nPath+"/"+instance, n1, n2, matrix1, matrix2, mapTable);
					 opt = problemParams(2);
					 bks = problemParams(3);
				} else if (problemParam == QA_PROBLEM){
					 //Console.OUT.println("n1 " + n1 + " n2 "+n2);				
					 // load flow and distance matrices for QAP 
					 SMTIAS.loadData(nPath+"/"+instance, n1, n1, matrix1, matrix2);
					 opt = problemParams(1);
					 bks = problemParams(2);
					 
					 Console.OUT.println(((opt < 0)?"bound ":"opt ")+Math.abs(opt)+" bks "+ bks);
					 opt = Math.abs(opt);
					 
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
						  Console.OUT.println("Target from CL: lower or equal than "+c);
					 } else 
					 {	
						  c = tCostFromCL * -1;
						  sl = true;
						  Console.OUT.println("Target from CL: strictly lower than "+c);
					 }
				} else // target cost loaded from file
				{
					 
					 sl = costFromF < 0; // strictly lower true for negative numbers
					 if ( costFromF == 1  || costFromF == -1  ) // try to get optimal cost
						  c = opt; 
					 else
						  c = bks;
					 Console.OUT.println("Target from file: "+(sl?"strictly lower than ":" lower or equal than ")+c);
				}
				
				val tCost = c >= 0 ? c : 0; // if negative cost put default value
				val sLow = sl;
				
				insNb++;
				
				if ( mode == 1 && outFormat == 1n )
					 Console.OUT.println("\n"+instance);
				printHeader(outFormat,problemParam);
				
				/**
				 *  2nd Loop: for repetition (testNb) 
				 */
				for (var j:Int = 1n ; j<=testNb; j++)
				{
					 var wallTime:Long = -System.nanoTime();	
					 val modelSeed = random.nextLong();
					 val prob = problemParam;
					 val cspGen = ():ModelAS(vectorSz)=>CSPProblem(prob).make (size as Long, 
								vectorSz, modelSeed, opts, matrix1, matrix2, mapTable);
					 
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
					 fWall += wallTime;
					 //Logger.debug(()=>{"wall time="+wtime/1e9});
					 
					 /**
					  *  Print stats for the current instance and sample
					  */
					 if(outFormat == 0n){
						  if (fileMode) Console.OUT.print(instance+",");
						  solvers().printStats(j,outFormat,problemParam);
						  Console.OUT.println(","+wallTime/1e9);
					 }
					 else if (outFormat == 1n){
						  Console.OUT.printf("\r");
						  solvers().printStats(j,outFormat,problemParam);
						  Console.OUT.printf(" %8.4f |\n",wallTime/1e9);
						  solvers().printAVG(j,outFormat,problemParam);
						  Console.OUT.flush();
					 }
					 
					 /**
					  *  Clear solvers data structures 
					  *  (to start the soving process with other instance)
					  */
					 finish for (p in Place.places()) at (p) {   
						  solvers().clear();
					 }	
					 
				} // End 2nd Loop
				
				/**
				 *   Print average of repetitions  
				 */
				if(outFormat == 0n){
					 Console.OUT.print(cFile+",");
					 solvers().printAVG(testNb,outFormat,problemParam);
					 Console.OUT.println(","+(fWall/(testNb*1e9)));
				}
				else if (outFormat == 1n){
					 Console.OUT.printf("\r");
					 printHeader(outFormat,problemParam);
					 solvers().printAVG(testNb,outFormat,problemParam);
					 Console.OUT.printf(" %8.4f |\n",(fWall/(testNb*1e9)));
					 //accStats.printAVG(testNo);
					 Console.OUT.printf("\n");
				}
				// Clear sample accumulator for repetitions
				solvers().clearSample();
				
				for(mline in matrix1) mline.clear();
				for(mline in matrix2) mline.clear();
		  }//End 1st Loop
		  
		  val avgWall = totalWallT/(insNb*testNb as Double);
		  totalTime += System.nanoTime();
		  
		  /**
		   *  Print General average if necessary (SMTI QAP)
		   */
		  if(outFormat == 0n){
				Console.OUT.print("TOTAL,");
				solvers().printGenAVG(insNb*testNb,outFormat,problemParam);
				Console.OUT.println(","+avgWall/1e9);
		  }else if (outFormat == 1n){
				//Console.OUT.println("|------------------------------------------------------------------------------------------------------------------------|");
				Console.OUT.println("\n   General Statistics for "+insNb+" problems, each one solved "+testNb+" times ");
				printHeader(outFormat,problemParam);
				solvers().printGenAVG(insNb*testNb,outFormat, problemParam);
				//accStats.printAVG(testNo);
				Console.OUT.printf(" %8.4f |\n",avgWall/1e9);
				Console.OUT.printf("\n");
		  }
		  
		  if(outFormat == 0n)
				Console.OUT.println("TotalInstances,"+insNb+",Repetitions,"+testNb+",Total_Time,"+(totalTime/1e9)+
						  ",AVG_wall_Time,"+(avgWall/1e9));
		  else if (outFormat == 1n)
				Console.OUT.println("TotalInstances: "+insNb+" Repetitions: "+testNb+" Total_Time: "+(totalTime/1e9)+
						  " AVG_wall_Time: "+(avgWall/1e9));	
		  
		  return;
	 }	
	 
	 static def printHeader(outF : Int, problem:Int){
		  // if(outF == 0n){
		  // 	Console.OUT.println("instance,count,time(s),iters,place,local_Min,swaps,resets,same/iter,restarts,blocking_pairs,singles,Changes,fRestP,fRestT,solution,walltime");
		  // }else 
		  if(outF == 1n)
		  {
				if (problem == Main.STABLE_MARRIAGE_PROBLEM || problem == Main.HOSPITAL_RESIDENT_PROBLEM)
				{	
					 Console.OUT.println("|-----------------------------------------------------------------------------------------------------------------------------------------------------------|");
					 Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  | frP-frT |  PS | TS |final cost|  gap  |   wtime  |");
					 Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|---------|-----|----|----------|-------|----------|");
				} else
				{
					 Console.OUT.println("|-----------------------------------------------------------------------------------------------------------------------------------------------|");
					 Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| Cng  | frP-frT |  PS | TS |final cost|  gap  |   wtime  |");
					 Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|------|---------|-----|----|----------|-------|----------|");
					 
				}
		  }
	 }
}
