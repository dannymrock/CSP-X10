package csp.solver; 
import csp.util.Logger;
import csp.util.Monitor;
import csp.util.Unit;
import csp.util.Utils;
import x10.util.Random;
import x10.util.StringUtil;
/**
 * Maintain a poolSize set of best partial solutions. These
 * can be queried by other places.
 * 
 */

public class ElitePool(sz:Long, poolSize:Int/*, seed:Long*/) {
	var nbEntries : Int = 0n;
	val bestPartialSolutions = new Rail(poolSize, CSPSharedUnit(sz,0n as Int,null,0n as Int)); // dummy value
	var random:Random = new Random();
	val monitor = new Monitor("ElitePool");
	
	val sortIndex = new Rail[Int](poolSize,-1n);
	
	// val solvers:PlaceLocalHandle[ParallelSolverI(sz)];
	// def this( sz:Long, poolSize: Int, ss: PlaceLocalHandle[ParallelSolverI(sz)]){
	// 	property(sz, poolSize);
	// 	solvers=ss;
	// }
	var distanceT:double = 0.5; 
	var distanceT2:double = 0.5; 
	
	public def setSeed(seed:Long){
		//monitor.atomicBlock(()=> {
		random=new Random(seed);
		//});
		val dStr = System.getenv("D");
		distanceT = (dStr==null)? 0.5 : StringUtil.parseLong(dStr)/100.0;
		
		val d2Str = System.getenv("D2");
		distanceT2 = (d2Str==null)? 0.5 : StringUtil.parseLong(d2Str)/100.0;
		//Console.OUT.println("Distance in the pool "+distanceT);		
	}
	
	
	/**
	 * Insert a copy of variables in the best partial solutions. Intended to be 
	 * invoked by solvers running at remote places.
	 * Note: Check that all calls are from remote places. If so the copy of
	 * variables will already have happened.
	 */
	public def tryInsertVector(cost:Long, variables:Rail[Int]{self.size==sz}, place:Int) {
		//monitor.atomicBlock(()=>tryInsertVector0(cost,variables,place));
		 monitor.atomicBlock(()=>tryInsertVector1(cost,variables,place));
	}

	//var countInsert:Int = 0n;
	protected def tryInsertVector0( cost : Long , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		 var victim:Int;
		 
		 if( nbEntries < poolSize ){
			  victim = nbEntries++;
		 }else{
			  // No place available select a victim
			  
			  victim = -1n;
			  
			  for (i in 0n..(nbEntries-1n)){
					if (cost < bestPartialSolutions(i).cost){
						 victim = i;
					} else if (cost == bestPartialSolutions(i).cost && compareVectors(variables, bestPartialSolutions(i).vector)){
						 victim = -1n;
						 break;
					}
			  }
		 }
		 if (victim >= 0n) {
			  //Console.OUT.println("insert vector with cost "+cost);	
			  bestPartialSolutions(victim) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
			  // countInsert++;
			  // if (countInsert % 10n == 0n){
			  // 	
			  // }
		 }
		 
		 return Unit();
	}
	//var countInsert:Int = 0n;
	protected def tryInsertVector1( cost : Long , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		 var worstConf:Long = -1; // index of the worst conf in the pool (highest cost)
		 var worstCost:Long = Long.MIN_VALUE;
		 var victim:Long = -1;
		 var simConf:Int = -1n;
		 var minDiff:Long = Long.MAX_VALUE;
		 
		 
		 if (nbEntries == 0n){  // I'm the first in the pool!
		   bestPartialSolutions(nbEntries++) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place); 
		 }else{
		   // Searching the worst conf (highest cost)
		   for ( i in 0n..(nbEntries-1n) ){
		 		// Select worst conf
		 		val thisCost = bestPartialSolutions(i).cost;
		 		if (thisCost > worstCost){
		 			 worstCost = thisCost;
		 			 worstConf = i;
		 		} 
		 		//select similar cost configuration
		 		val cdiff = Math.abs(thisCost - cost);
		 		if (cdiff < minDiff){
		 			 minDiff = cdiff;
		 			 simConf = i; 
		 		}	 
		   }
		   
		   // Replace the worst conf in the pool with a new one
		   if (cost < worstCost && distance(variables, bestPartialSolutions(simConf).vector) >= distanceT)
		 		if (nbEntries < poolSize )
		 			 victim = nbEntries++;
		 		else{
		 			 //if (distance(variables, bestPartialSolutions(simConf).vector) >= distanceT )
		 			 victim = worstConf;					 
		 		}
		   if (victim >= 0n)
		 		bestPartialSolutions(victim) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
		 }
		 return Unit();
	}
	
	
	/**
	 * Insert a copy of variables in the best partial solutions. Intended to be 
	 * invoked by solvers running at remote places.
	 * Note: Check that all calls are from remote places. If so the copy of
	 * variables will already have happened.
	 */
	public def tryInsertLM(cost:Long, variables:Rail[Int]{self.size==sz}, place:Int) {
		 monitor.atomicBlock(()=>tryInsertLM0(cost,variables,place));		
	}

	//var countInsert:Int = 0n;
	//val countLM = new Rail[Int](poolSize,1n);
	protected def tryInsertLM0( cost : Long , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		 // Insert count similar conf 
		 // var worstConf:Int = -1n; // index of the worst conf in the pool (highest cost)
		 // var worstCost:Int = Int.MIN_VALUE;
		 // var victim:Int = -1n;
		 // 
		 // var eqConf:Int = -1n;
		 // var minDiff:Int = Int.MAX_VALUE;
		 // 
		 // 
		 // if (nbEntries == 0n){
			//   countLM(nbEntries) = 1n;
			//   bestPartialSolutions(nbEntries++) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
		 // }else{
			//   // Searching the worst conf (highest cost)
			//   for ( i in 0n..(nbEntries-1n) ){
			// 		// Select worst conf
			// 		val thisCost = bestPartialSolutions(i).cost;
			// 		if (thisCost > worstCost){
			// 			 worstCost = thisCost;
			// 			 worstConf = i;
			// 		} 
			// 		
			// 		//select similar cost configuration
			// 		if(thisCost  == cost){ 
			// 			 eqConf = i; 
			// 			// Console.OUT.println("Equal");
			// 		}	 
			//   }
			//   
			//   if (eqConf != -1n && compareVectors(variables, bestPartialSolutions(eqConf).vector)){
			// 		countLM(eqConf) = countLM(eqConf)+1n;
			// 		//Console.OUT.println("Equal");
			// 		return Unit();
			//   }
			// 		
			//   // Replace the worst conf in the pool with a new one
			//   if (cost < worstCost)
			// 		if (nbEntries < poolSize )
			// 			 victim = nbEntries++;
			// 		else
			// 			 victim = worstConf;
			//   
			//   if (victim >= 0n){
			// 		bestPartialSolutions(victim) = 
			// 			 new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
			// 		countLM(victim) = 1n;
			// 		//Console.OUT.println("Replace "+victim);
			//   }
			// 	
		 // }
		 // return Unit();
		 /*****/
		 
		 var worstConf:Long = -1; // index of the worst conf in the pool (highest cost)
		 var worstCost:Long = Long.MIN_VALUE;
		 var victim:Long = -1;
		 
		 var simConf:Int = -1n;
		 var minDiff:Long = Long.MAX_VALUE;
		 
		 
		 if (nbEntries == 0n){
			  bestPartialSolutions(nbEntries++) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place); 
		 }else{
			  // Searching the worst conf (highest cost)
			  for ( i in 0n..(nbEntries-1n) ){
					// Select worst conf
					val thisCost = bestPartialSolutions(i).cost;
					if (thisCost > worstCost){
						 worstCost = thisCost;
						 worstConf = i;
					} 
					
					//select similar cost configuration
					val cdiff = Math.abs(thisCost - cost);
					if (cdiff < minDiff){
						 minDiff = cdiff;
						 simConf = i; 
					}	 
			  }
			  
			  // Replace the worst conf in the pool with a new one
			  if (cost < worstCost && distance(variables, bestPartialSolutions(simConf).vector) >= distanceT2)
					if (nbEntries < poolSize )
						 victim = nbEntries++;
					else{
						 //if (distance(variables, bestPartialSolutions(simConf).vector) >= distanceT )
						 victim = worstConf;					 
					}
			  
			  if (victim >= 0n)
					bestPartialSolutions(victim) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
		 }
		 return Unit();
		 
		 /****/
		 // var victim:Int;
		 // 
		 // // check distances
		 // 
		 // 
		 // if( nbEntries == 0n ){
			//   victim = nbEntries++;
		 // }else{
			//   // No place available select a victim
			//   
			//   victim = -1n;
			//   
			//   for (i in 0n..(nbEntries-1n)){
			// 		if (cost <= bestPartialSolutions(i).cost){
			// 			 victim = i;
			// 		} 
			// 		else if (cost == bestPartialSolutions(i).cost && distance(variables, bestPartialSolutions(i).vector) > 0.5){
			// 			 victim = -1n;
			// 			 break;
			// 		}
			//   }
		 // }
		 // if (victim >= 0n) {
			//   bestPartialSolutions(victim) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
		 // }
		 // 
		 // return Unit();
	}
	
	
	public def distance(conf1 : Valuation(sz), conf2 : Valuation(sz)) : Double {
		 var count : Int = 0n;
		 for (i in 0n..(sz as Int - 1n)){
			  //Logger.debug("comparing: "+conf1(i)+" - "+conf2(i));
			  if(conf1(i) != conf2(i)) count++; 
		 }
		 val dis = count as Double / sz;
		 //Console.OUT.println("distance in Pool = "+dis);
		 return dis;
	}
	
	
	public static def compareVectors (vec1 : Rail[Int], vec2 : Rail[Int]):Boolean{
		for (i in 0..( vec1.size-1))
			if(vec1(i) != vec2(i)) return false;
		return true;
	}
	
	public def printVectors(){
		for(i in 0..(nbEntries-1)) {
			Console.OUT.print(i+". Cost = "+bestPartialSolutions(i).cost+" place "+bestPartialSolutions(i).place); //+" count "+countLM(i));
			Utils.show(" Vector",bestPartialSolutions(i).vector);
		}
	}
	
	/**
	 * Get some vector from the best solutions.
	 */
	public def getRandomConf():Maybe[CSPSharedUnit(sz)]=
		monitor.atomicBlock(()=> {
			//if (here.id==0)Console.OUT.println(here+"aqui");
			if (nbEntries < 1n) return null;
			val index = random.nextInt(nbEntries);
			//if (index >= nbEntries) Console.OUT.println("Golden: index is " + index + " needed to be < " + nbEntries);
			//if (here.id==0)Console.OUT.println(here+"alli");
			return new Maybe(bestPartialSolutions(index));
		});
	
	public def getBestConf():Maybe[CSPSharedUnit(sz)]=
		monitor.atomicBlock(()=> {
			if (nbEntries < 1n) return null;
			var bcost:Long = Long.MAX_VALUE;
			var victim:Long = -1;
			for (i in 0n..(nbEntries-1n)){
				if (bestPartialSolutions(i).cost < bcost){
					bcost = bestPartialSolutions(i).cost;
					victim = i;
				}
			}
			return new Maybe(bestPartialSolutions(victim));
		});
	  
		
	public def clear(){
	    monitor.atomicBlock(()=> {
	        nbEntries = 0n;
	        Unit()
	    });
	}
	
}

