package csp.model;
import csp.solver.ParallelSolverI;
import csp.solver.PlacesMultiWalks;
import csp.util.Logger;
import x10.util.Team;

/** Main
 * 	Main class of the project. 
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013 	-> First Version
 */

import x10.util.OptionsParser;
import x10.util.Option;
import x10.util.Random;
import x10.compiler.Pragma;
import x10.io.File;
import x10.io.FileReader;
import x10.io.FileWriter;


public class Main {
	
	public static struct CSPProblem(kind:Int) {
		public def make(size:Long, vectorSize:Long, seed:Long):ModelAS(vectorSize) {
			if (kind==MAGIC_SQUARE_PROBLEM) return new MagicSquareAS(size as Int, vectorSize, seed);
			if (kind==COSTAS_PROBLEM) return new CostasAS(vectorSize, seed);
			if (kind==ALL_INTERVAL_PROBLEM) return new AllIntervalAS(vectorSize, seed, true);
			if (kind==LANGFORD_PROBLEM) return new LangfordAS(size, vectorSize, seed);
			//if (kind==STABLE_MARRIAGE_PROBLEM) return new StableMarriageAS(vectorSize, seed);
			return new PartitAS(vectorSize, seed);
		}
		// public def make_SM(size:Long, vectorSize:Long, seed:Long, mPrefs:Rail[Rail[Int]],wPrefs:Rail[Rail[Int]])
		// :ModelAS(vectorSize) {
		// 	return new StableMarriageAS(vectorSize, seed, mPrefs, wPrefs);
		// }
	}
	
	public static val UNKNOWN_PROBLEM=0n;
	public static val MAGIC_SQUARE_PROBLEM = 1n;
	public static val COSTAS_PROBLEM = 2n;
	public static val ALL_INTERVAL_PROBLEM = 3n;
	public static val LANGFORD_PROBLEM = 4n;
	public static val PARTIT_PROBLEM = 5n;
	//public static val STABLE_MARRIAGE_PROBLEM = 6n;
	
	var fp  : File;
	
	public static def main(args:Rail[String]):void{
		var totalTime:Long = -System.nanoTime();
		//val r = new Random();
		val opts = new OptionsParser(args, new Rail[Option](0L), [
		                                                          Option("l", "", "restart Limit"),
		                                                          Option("s", "", "Size of the problem"),
		                                                          Option("b", "", "Number of benchmark tests"),
		                                                          Option("m", "", "Solver mode distribution 0 for Places \"n\" for Activities (n number of activities). Default 0."),
		                                                          Option("t", "", "probability p2 - ties."),
		                                                          Option("c", "", "Probability to change vector in Intra-Team Communication "),
		                                                          Option("R", "", "Intra-team Communication Interval for Receive (iterations) . Default 0 - no communication."),
		                                                          Option("S", "", "Intra-team Communication Interval for Send (iterations) . Default 0 - no communication."),
		                                                          Option("I", "", "Inter-team Communication Interval (tim miliseconds) . Default 0 - no communication."),
		                                                          Option("n", "", "nodes_per_team parameter. Default 4."),
		                                                          Option("k", "", "poolsize."),
		                                                          Option("y", "", "seed. Default random"),
		                                                          Option("d", "", "minimum permisible distance."),
		                                                          Option("p", "", "csp problem"),
		                                                          Option("o", "", "output format: machine 0, info 1")
		                                                          ]);
		
		val cspProblem  = opts("-p", "MSP");
		val restLimit	= opts("-l", 1000000000n);
		val size		= opts("-s", 10n);
		val testNo		= opts("-b", 10n);
		val solverMode	= opts("-m", 0n);
		val p2			= opts("-t", 50n);
		val changeProb	= opts("-c", 100n);
		val intraTIRecv	= opts("-R", 0n);
		val intraTISend = opts("-S", 0n);
		val interTI		= opts("-I", 0);
		val nodesPTeam	= opts("-n", 1n);
		val poolSize	= opts("-k", 4n);
		val inSeed		= opts("-y", 0);
		val minDistance	= opts("-d", 0.3);
		//var path:String	= opts("-p", "");
		val outFormat	= opts("-o", 1n);
		
		var vectorSize:Long=0;
		
		//at(Main.param) Main.param().poolSize = poolSize;
		
		if (outFormat == 0n){
			Console.OUT.println("Path,size,samples,mode,probChangeVector,intra-Team Recv,intra-Team Send,inter-Team,minDistance,poolsize,places,nodes_per_team,seed");
			Console.OUT.println(cspProblem+","+size+","+testNo+","+(solverMode==0n ?"seq":"parallel")+","+changeProb+","+
					intraTIRecv+","+intraTISend+","+interTI+","+minDistance+","+poolSize+","+Place.MAX_PLACES+","+nodesPTeam
					+","+inSeed+"\n");
		}
		else if(outFormat == 1n){
			Console.OUT.println("Problem: "+cspProblem+"\nSize: "+size+"\nNumber of repetitions: "+testNo
					+"\nSolverMode: "+(solverMode==0n ?"seq":"parallel")+"\nProbability to Change Vector: "
					+changeProb+"\nIntra-Team Comm. inteval Recv: "+intraTIRecv+" iterations"
					+"\nIntra-Team Comm. inteval Send: "+intraTISend+" iterations"
					+"\nInter-Team Comm. inteval: "+interTI+" ms"+"\nMinimum permissible distance: "
					+minDistance+"\nPool Size: "+poolSize);
			
			Console.OUT.println("Using multi-walks with "+Place.MAX_PLACES+" Places");
			Console.OUT.println("There are "+Place.MAX_PLACES/nodesPTeam+" teams each one with "+nodesPTeam+" explorer places. "+
					Place.MAX_PLACES+" explorers in total (places)");
			
		}
		
		vectorSize=size;
		
		var param : Int = UNKNOWN_PROBLEM;
		if (cspProblem.equalsIgnoreCase("MSP")) {
			Logger.debug(()=>{"Magic Square Problem"});
			param = MAGIC_SQUARE_PROBLEM;
			vectorSize= size*size;
		} else if(cspProblem.equals("CAP")){
			Logger.debug(()=>{"Costas Array Problem"});
			param = COSTAS_PROBLEM;
			vectorSize= size;
		} else if(cspProblem.equals("AIP")){
			Logger.debug(()=>{"All-Interval Array Problem"});
			param = ALL_INTERVAL_PROBLEM;
			vectorSize=size;
		} else if(cspProblem.equals("LNP")){
			Logger.debug(()=>{"Langford Pairing Problem"});
			param = LANGFORD_PROBLEM;
			vectorSize=2*size;
		} else if(cspProblem.equals("NPP")){
			Logger.debug(()=>{"Number Partition Problem"});
			param = PARTIT_PROBLEM;
			vectorSize=size;
		} else /* if(cspProblem.equals("SMP")){
			 * Logger.debug(()=>{"Stable Marriage Problem"});
			 * param = STABLE_MARRIAGE_PROBLEM;
			 * vectorSize=size;
		 * } else*/{
			Console.OUT.println("Error: Type a valid CSP example: MSP, CAP, AIP, LNP, NPP o SMP"); 
			return;
		}
		
		/*
		 *  Creating objects for solver execution
		 */
		val vectorSz = vectorSize;
		
		val solvers:PlaceLocalHandle[ParallelSolverI(vectorSz)];	
		solvers = PlaceLocalHandle.make[ParallelSolverI(vectorSz)](PlaceGroup.WORLD, 
				()=>new PlacesMultiWalks(vectorSz, intraTIRecv, intraTISend, interTI, poolSize, nodesPTeam,
						changeProb, minDistance) as ParallelSolverI(vectorSz));
		
		if (outFormat == 0n){
			Console.OUT.println("file,count,time(s),iters,place,local_Min,swaps,resets,same/iter,restarts,blocking_pairs,singles,Changes,force_restart,solution,walltime");
		}
		else if(outFormat == 1n){
			Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  |  FR |  PS | walltime");
			Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
		}
		
		// Installing solvers
		var totalInTime :Long = -System.nanoTime();
		finish for (p in Place.places()) at (p) async{	
			solvers().installSolver(solvers);
		}
		val intime = totalInTime += System.nanoTime();
		Logger.info(()=>{"install time: "+ intime/1e9});
		
		// accumulated times
		var totalExTimes :Long = 0;
		var totalLdTimes :Long = 0;
		var totalClearTimes :Long = 0;
		
		// seed
		val seed = inSeed;//(inSeed == 0) ? j as Long:inSeed;
		val random = new Random(seed);
		
		var j:Int = 0n; //counter of samples
		
		
		
		
		var samplesNb:Int = 0n;
		// val mPref:Rail[Rail[Int]] = new Rail[Rail[Int]](vectorSz, (Long) => new Rail[Int](vectorSz,0n));
		// val wPref:Rail[Rail[Int]] = new Rail[Rail[Int]](vectorSz, (Long) => new Rail[Int](vectorSz,0n));
		
		//solve the problem "testNo" times
		for (j=1n ; j<=testNo; j++){
			val problem=param;
			var extTime:Long = -System.nanoTime();

            val modelSeed = random.nextLong();
			val cspGen=():ModelAS(vectorSz)=> CSPProblem(problem).make(size as Long, vectorSz, modelSeed);
			
			//cspGen=():ModelAS(vectorSz)=> new ModelAS(sizeF as Long, modelSeed, mPref, wPref,restLimit) as ModelAS(vectorSz);
			if (solverMode == 0n){
				finish for (p in Place.places()) {
					val solverSeed = random.nextLong();	
					at (p) async
					solvers().solve(solvers, cspGen, solverSeed);	
				}
			}else{
				finish for(var i:Long=Place.MAX_PLACES-1; i>=0; i-=16) at	(Place(i)) async {
					val max = here.id; val min = Math.max(max-15, 0);
					val r = new Random(random.nextLong()+here.id);
					finish for(k in min..max){
						val solverSeed = r.nextLong();
						at(Place(k)) async	solvers().solve(solvers, cspGen, solverSeed);
					}
				}
			}
			Logger.debug(()=>" Main: End solve function  in all places ");
			
			// Detect if there is no winner
			solvers().verifyWinner(solvers);
			
			extTime += System.nanoTime();
			val extt = extTime;
			totalExTimes += extTime;
			Logger.debug(()=>{"ext Time="+extt/1e9});
			
			if(outFormat == 0n){
				//Console.OUT.print(file+",");
				solvers().printStats(j,outFormat);
				Console.OUT.println(","+extTime/1e9);
			}
			else if (outFormat == 1n){
				Console.OUT.printf("\r");
				solvers().printStats(j,outFormat);
				Console.OUT.printf(" %3.2f \n",extTime/1e9);
				solvers().printAVG(j,outFormat);
				Console.OUT.flush();
			}
			var clearTime:Long = -System.nanoTime();
			finish for (p in Place.places()) at (p) async{	
				solvers().clear();
			}
			val cltime=clearTime += System.nanoTime();
			totalClearTimes += clearTime;
			Logger.debug(()=>{" cleartime="+cltime/1e9});
			Logger.debug(()=>" Start broadcatFlat: solvers().clear function ");
			
			//System.sleep(1000);
		}
		if(outFormat == 0n){
			//Console.OUT.print(file+",");
			solvers().printAVG(testNo,outFormat);
		}
		else if (outFormat == 1n){
			Console.OUT.printf("\r");
			Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
			Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  |  FR |  PS |");
			Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
			solvers().printAVG(testNo,outFormat);
			//accStats.printAVG(testNo);
			Console.OUT.printf("\n");
		}
		// Clear sample accumulator
		solvers().clearSample();
		
		val avgld = totalLdTimes/(testNo as Double); 
		val avgclear = totalClearTimes/(testNo as Double); 
		val avgext=totalExTimes/(testNo as Double);
		totalTime += System.nanoTime();
		if(outFormat == 0n){
			Console.OUT.println("Total_Files,Repetition_per_File,Total_Time,AVG_Loading_Time,AVG_external_solving_Time,AVG_clear_Time");
			Console.OUT.println(samplesNb+","+testNo+","+(totalTime/1e9)+","+(avgld/1e9)+","+(avgext/1e9)+","+(avgclear/1e9));
		}
		else if (outFormat == 1n)
			Console.OUT.println("Total Time= "+(totalTime/1e9)+" AVG external solving Time= "+(avgext/1e9)+" AVG clear Time= "+(avgclear/1e9));
		
		return;
	}	
}
