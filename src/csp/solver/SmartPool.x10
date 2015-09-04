package csp.solver;
import csp.util.Logger;
import csp.util.Monitor;
import csp.util.Unit;
import csp.util.Utils;
import x10.util.Random;
import x10.util.StringUtil;

/**
 * Class SmartPool
 */
public class SmartPool(sz:Long, poolSize:Int) {
	 // Number of entries on the short, medium and long memory arrays
	 //var nSM : Int = 0n;
	 //var nMM : Int = 0n;
	 //var nLM : Int = 0n;
	 val nbEntries = new Rail[Int](3, 0n);
	 val pool = new Rail[Rail[CSPSharedUnit]](3);
	 
	 
	 //val shortMem = new Rail(poolSize, CSPSharedUnit(sz,0n as Int,null,0n as Int)); 
	 //val mediumMem = new Rail(poolSize, CSPSharedUnit(sz,0n as Int,null,0n as Int)); 
	 //val longMem = new Rail(poolSize, CSPSharedUnit(sz,0n as Int,null,0n as Int)); 
	 var random:Random = new Random();
	 val monitor = new Monitor("ElitePool");
	 var distanceT:double = 0.5; 
	 var distanceT2:double = 0.5; 
	 var nInsert:Int = 0n;
	 
	 public def this(sz:Long, poolSize:Int){
		  property(sz, poolSize);
		  //pool = new Rail[Rail[CSPSharedUnit]](3, (Long)=> new Rail(poolSize, CSPSharedUnit(sz,0n as Int,null,0n as Int))); 
		  for (i in  0..2 )
				pool(i) = new Rail[CSPSharedUnit](poolSize, CSPSharedUnit(sz,0n as Int,null,0n as Int));
	 }
	 
	 public def setSeed(seed:Long){
		  this.random = new Random(seed);
		  val dStr = System.getenv("D1");
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
		  monitor.atomicBlock(()=>tryInsertVector0(cost,variables,place));
		  //monitor.atomicBlock(()=>tryInsertVector1(cost,variables,place));
	 }
	 
	 public def insert( poolN:Int, dist:Double, cost:Long , variables:Rail[Int]{self.size==sz}, place:Int ):CSPSharedUnit {
		  var worstConf:Long = -1; // index of the worst conf in the pool (highest cost)
		  var worstCost:Long = Long.MIN_VALUE;
		  var simConf:Int = -1n;
		  var minDiff:Long = Long.MAX_VALUE;
		  
		  // Searching the worst conf (highest cost)
		  if (nbEntries(poolN) == 0n){  // I'm the first in the pool!
				pool(poolN)(nbEntries(poolN)++) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
				return new CSPSharedUnit( sz, 0n as Int, null, -1n as Int);
		  }else{
				for ( i in 0n..(nbEntries(poolN)-1n) ){
					 // Select worst conf
					 val thisCost = pool(poolN)(i).cost;
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
				if (nbEntries(poolN) < poolSize && cost < worstCost && distance(variables, pool(poolN)(simConf).vector) >= dist ){
					 pool(poolN)(nbEntries(poolN)++) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
					 return new CSPSharedUnit( sz, 0n as Int, null, -1n as Int);
				}
				
				if (worstConf >= 0n && cost < worstCost && distance(variables, pool(poolN)(simConf).vector) >= dist){
					 val victim = pool(poolN)(worstConf);
					 pool(poolN)(worstConf) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
					 return victim;
				}
		  }
		  return new CSPSharedUnit( sz, 0n as Int, null, -1n as Int);
	 }
	 
	 protected def tryInsertVector0( cost : Long , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		  // try to insert conf in short term pool
		  val victimShort = insert(0n, 0.3, cost, variables, place);
		  if ( victimShort.place > 0 ){//&& random.nextDouble() < 0.8 ){
				val victimMedium = insert(1n, 0.6, victimShort.cost, victimShort.vector, victimShort.place);
				if ( victimMedium.place > 0){// && random.nextDouble() < 0.9){
					 insert(2n, 0.9, victimMedium.cost, victimMedium.vector, victimMedium.place);
				}
		  }
		  return Unit();
	 }
	 
	 protected def tryInsertVector1( cost : Long , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		  var worstConf:Long = -1; // index of the worst conf in the pool (highest cost)
		  var worstCost:Long = Long.MIN_VALUE;
		  var victim:Long = -1;
		  var simConf:Int = -1n;
		  var minDiff:Long = Long.MAX_VALUE;
		  
		  // select the type meory to use
		  var pindex : Int = -1n;
		  if (this.nInsert % 20n == 0n){
				pindex = 2n;
		  } else if(this.nInsert %5n == 0n){
				pindex = 1n;
		  }else{
				pindex = 0n;
		  }
		  
		  nInsert++;
		 	  	  
		  if (this.nbEntries(pindex) == 0n){  // I'm the first in the pool!
				pool(pindex)(this.nbEntries(pindex)++) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place); 
		  }else{
				// Searching the worst conf (highest cost)
				for ( i in 0n..(nbEntries(pindex)-1n) ){
					 // Select worst conf
					 val thisCost = pool(pindex)(i).cost;
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
				if (cost < worstCost && distance(variables, pool(pindex)(simConf).vector) >= distanceT)
					 if (nbEntries(pindex) < poolSize )
						  victim = nbEntries(pindex)++;
					 else{
						  //if (distance(variables, bestPartialSolutions(simConf).vector) >= distanceT )
						  victim = worstConf;					 
					 }
				if (victim >= 0n)
					 pool(pindex)(victim) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
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

	 protected def tryInsertLM0( cost : Long , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		  // var worstConf:Long = -1; // index of the worst conf in the pool (highest cost)
		  // var worstCost:Long = Long.MIN_VALUE;
		  // var victim:Long = -1;
		  // 
		  // var simConf:Int = -1n;
		  // var minDiff:Long = Long.MAX_VALUE;
		  // 
		  // 
		  // if (nbEntries == 0n){
				// bestPartialSolutions(nbEntries++) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place); 
		  // }else{
				// // Searching the worst conf (highest cost)
				// for ( i in 0n..(nbEntries-1n) ){
				// 	 // Select worst conf
				// 	 val thisCost = bestPartialSolutions(i).cost;
				// 	 if (thisCost > worstCost){
				// 		  worstCost = thisCost;
				// 		  worstConf = i;
				// 	 } 
				// 	 
				// 	 //select similar cost configuration
				// 	 val cdiff = Math.abs(thisCost - cost);
				// 	 if (cdiff < minDiff){
				// 		  minDiff = cdiff;
				// 		  simConf = i; 
				// 	 }	 
				// }
				// 
				// // Replace the worst conf in the pool with a new one
				// if (cost < worstCost && distance(variables, bestPartialSolutions(simConf).vector) >= distanceT2)
				// 	 if (nbEntries < poolSize )
				// 		  victim = nbEntries++;
				// 	 else{
				// 		  //if (distance(variables, bestPartialSolutions(simConf).vector) >= distanceT )
				// 		  victim = worstConf;					 
				// 	 }
				// 
				// if (victim >= 0n)
				// 	 bestPartialSolutions(victim) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
		  // }
		  return Unit();
	 }
	 
	 
	 
	 /**
	  *  Distance function
	  *  all equal = 0
	  *  all diff  = 1
	  */
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
		  for (i in 0n..2n)
				for (j in 0..(nbEntries(i)-1)) {
					 Console.OUT.print((i==2n?"long ":i==1n?"med ":"short ")+j+". Cost = "+pool(i)(j).cost+" place "+pool(i)(j).place); //+" count "+countLM(i));
					 Utils.show(" Vector",pool(i)(j).vector);
		  }
	 }
	 
	 /**
	  * Get some vector from the best solutions.
	  */
	 public def getRandomConf():Maybe[CSPSharedUnit(sz)]=
		  monitor.atomicBlock(()=> {
				//if (here.id==0)Console.OUT.println(here+"aqui");
				//Console.OUT.println("s "+nbEntries(0)+"m "+nbEntries(1)+"l "+nbEntries(2));
				val totalEn = nbEntries(0)+nbEntries(1)+nbEntries(2);
				if (totalEn < 1n) return null;
				
				// //Select the mem
				// var mem:Int = random.nextInt(3n);
				// while (nbEntries(mem) == 0n){
				// 	 mem = random.nextInt(3n);
				// }	
				// //select the index
				// val index = random.nextInt(nbEntries(mem));
				
				var index:Int; // = random.nextInt(totalEn)+1n;
				// Console.OUT.println("initial value "+index);
				var mem:Int=0n; 
				
				
				if (nbEntries(0)>0 && nbEntries(1)>0 && nbEntries(2)>0){
					 val pooln = random.nextInt(10n);
					 if (pooln < 5n)
						  mem = 0n;
					 else if(pooln < 8)
						  mem = 1n;
					 else
						  mem = 2n;
					 
					 index = random.nextInt(nbEntries(mem))+1n;
				} else {	 
					 index = random.nextInt(totalEn)+1n;
					 for(mem = 0n; index > nbEntries(mem); mem++){
						  index -= nbEntries(mem);
					 }
				}
				
				//Console.OUT.println("mem "+mem+" index "+index);
				
				//if (index >= nbEntries) Console.OUT.println("Golden: index is " + index + " needed to be < " + nbEntries);
				//if (here.id==0)Console.OUT.println(here+"alli");
				return new Maybe(pool(mem)(index-1) as CSPSharedUnit(sz) );
		  });
	 
	 public def getBestConf():Maybe[CSPSharedUnit(sz)]=
		  monitor.atomicBlock(()=> {
				if (nbEntries(0) < 1n) return null;
				var bcost:Long = Long.MAX_VALUE;
				var victim:Long = -1;
				for (i in 0n..(nbEntries(0)-1n)){
					 if (pool(0)(i).cost < bcost){
						  bcost = pool(0)(i).cost;
						  victim = i;
					 }
				}
				return new Maybe(pool(0)(victim) as CSPSharedUnit(sz));
		  });
	 
	 
	 public def clear(){
		  monitor.atomicBlock(()=> {
				for (i in nbEntries.range())
					 nbEntries(i) = 0n;
				Unit()
		  });
	 }
}