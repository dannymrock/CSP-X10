package csp.solver;
import csp.util.Logger;
import csp.util.Monitor;
import csp.util.Unit;
import csp.util.Utils;
import x10.util.Random;
import x10.util.StringUtil;
import csp.model.ParamManager;
import x10.util.StringBuilder;

/**
 * Class SmartPool
 */
public class SmartPool(sz:Long, poolSize:Int) {
	 // Number of entries on the short, medium and long memory arrays
	
	 // three level pool: high, medium and low quality configurations
	 public static val HIGH=0n;
	 public static val MEDIUM=1n;
	 public static val LOW=2n;
	 
	 public static val SMART = 1;
	 
	 // Three level Pool 
	 protected val nbEntries = new Rail[Int](3, 0n);
	 protected val pool = new Rail[Rail[CSPSharedUnit(sz)]](3);
	 protected val poolMode:Long;
	 
	 protected var random:Random = new Random();
	 protected val monitor = new Monitor("SmartPool");
	 protected var distance:double; 
	 
	 public def this(sz:Long, pSize:Int, pMode:Long, minDist:Double){
		  property(sz, pSize);
		  poolMode = pMode;
		  distance = minDist;		  
		  for (i in 0..2)
				pool(i) = new Rail[CSPSharedUnit(sz)](poolSize, CSPSharedUnit(sz,0n,null,0n));
	 }
	 
	 public def setSeed(seed:Long){
		  this.random = new Random(seed);		
	 }
	 
	 /**
	  * Insert a copy of variables in the best partial solutions. Intended to be 
	  * invoked by solvers running at remote places.
	  * Note: Check that all calls are from remote places. If so the copy of
	  * variables will already have happened.
	  */
	 public def tryInsertConf(cost:Long, variables:Rail[Int]{self.size==sz}, place:Int) {
		  monitor.atomicBlock(()=>tryInsertConf0(cost,variables,place));
	 }
	 
	 protected def tryInsertConf0( cost : Long , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		  // TODO Closure	  
		  if (poolMode == SMART)
				return smartInsert(cost, variables, place);
		  else
				return normalInsert(cost, variables, place);
	 }
	 
	 /**
	  * Generic funtion to insert a configuration on the smart pool
	  * @param poolType insert the incoming configuration on the poolType
	  *                 pool (HIGH, MEDIUM, LOW)
	  * @param dist Minimum distance allowed to insert the configuration
	  * @param cost Cost of the incomming configuration
	  * @param variables The incomming configuration
	  * @param place Origin place number of the incomming configuration
	  * @return The victim configuration, if it exists (dummy value othercase place = -1)
	  */
	 protected def insert( poolType:Int, dist:Double, cost:Long, variables:Rail[Int]{self.size==sz}, place:Int ):CSPSharedUnit {
		  var worstConf:Long = -1; // index of the worst conf in the pool (highest cost)
		  var worstCost:Long = Long.MIN_VALUE;
		  var simConf:Int = -1n;
		  var minDiff:Long = Long.MAX_VALUE;
		  
		  // Searching the worst conf (highest cost)
		  if (this.nbEntries(poolType) == 0n){  // I'm the first in the pool!
				pool(poolType)(nbEntries(poolType)++) = 
					 new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
				// Return dummy value, there isn't victim
				return new CSPSharedUnit( sz, 0n, null, -1n);
		  }else{
			   for ( var i:Int = 0n; i < this.nbEntries(poolType); i++){
					 // Select worst conf
					 val thisCost = pool(poolType)(i).cost;
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
				if (this.nbEntries(poolType) < this.poolSize && cost < worstCost && 
						  distance(variables, pool(poolType)(simConf).vector) >= dist ){
					 pool(poolType)(this.nbEntries(poolType)++) = 
						  new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
					 return new CSPSharedUnit( sz, 0n, null, -1n);
				}
				
				if (worstConf >= 0n && cost < worstCost && 
						  distance(variables, pool(poolType)(simConf).vector) >= dist){
					 val victim = pool(poolType)(worstConf);
					 pool(poolType)(worstConf) = 
						  new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
					 return victim;
				}
		  }
		  return new CSPSharedUnit( sz, 0n, null, -1n);
	 }
	 
	 /**
	  * Try to insert configuration using the "Smart Pool"
	  * @param cost Cost of the incomming configuration
	  * @param variables The incomming configuration
	  * @param place Origin place number of the incomming configuration
	  * @return Unit structure (necessary to the proper operation of the monitor)
	  */
	 protected def smartInsert( cost : Long , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		  Logger.info(()=>{"Smart Pool: Smart Insert"});
		  // try to insert conf in high quality pool - min distance allowed 0.3
		  val victimShort = insert(this.HIGH, 0.3, cost, variables, place);
		  //if place == -1 then it is a dummy value (there's no victim)
		  if ( victimShort.place > 0 ){ 
				// try to insert conf in medium quality pool - min distance allowed 0.6
				val victimMedium = insert(this.MEDIUM, 0.6, victimShort.cost, victimShort.vector as Valuation(sz), victimShort.place);
				if ( victimMedium.place > 0){
					 // try to insert conf in low quality pool - min distance allowed 0.9
					 insert(this.LOW, 0.9, victimMedium.cost, victimMedium.vector as Valuation(sz), victimMedium.place);
				}
		  }
		  return Unit();
	 }

	 /**
	  * Try to insert configuration using the "Elite Pool"
	  * Using only the HIGH quality memory
	  * @param cost Cost of the incomming configuration
	  * @param variables The incomming configuration
	  * @param place Origin place number of the incomming configuration
	  * @return Unit structure (necessary to the proper operation of the monitor)
	  */
	 protected def normalInsert( cost : Long , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		  Logger.info(()=>{"Smart Pool: normal Insert"});
		  insert(HIGH, distance, cost, variables, place);
		  return Unit();
	 }
	 
	 /**
	  *  Distance function
	  * Compute the distance between two configurations (0..1)  
	  * If distance = 0, the two confs are pairwise equal
	  * If distance = 1, the two confs are pairwise totally different
	  * @param conf1 first configuration to test
	  * @param conf2 second configuration to test
	  * @return pairwise distance between conf1 and conf2
	  */
	 protected def distance(conf1 : Valuation(sz), conf2 : Valuation(sz)) : Double {
		  var count : Int = 0n;
		  for (i in 0n..(sz as Int - 1n)){
				//Logger.debug("comparing: "+conf1(i)+" - "+conf2(i));
				if(conf1(i) != conf2(i)) count++; 
		  }
		  val dist = count as Double / sz;
		  //Console.OUT.println("distance in Pool = "+dis);
		  return dist;
	 }
	 
	 protected static def compareVectors (vec1 : Rail[Int], vec2 : Rail[Int]):Boolean{
		  for (i in 0..( vec1.size-1))
				if(vec1(i) != vec2(i)) return false;
		  return true;
	 }
	 
	 public def printVectors(){
		  for (i in 0n..2n)
				for (j in 0..(nbEntries(i)-1)) {
					 Console.OUT.print((i==2n?"long ":i==1n?"med ":"short ")+j+". Cost = "+
								pool(i)(j).cost+" place "+ pool(i)(j).place); //+" count "+countLM(i));
					 Utils.show(" Vector",pool(i)(j).vector);
		  }
	 }
	 
	 /**
	  * Get a smart configuration from HIGH, MEDIUM or LOW quality pool.
	  */
	 public def getPConf():Maybe[CSPSharedUnit(sz)]=
		  monitor.atomicBlock(()=> {
				//Console.OUT.println("s "+nbEntries(0)+"m "+nbEntries(1)+"l "+nbEntries(2));
				val totalEn = this.nbEntries(HIGH) + this.nbEntries(MEDIUM) + 
				              this.nbEntries(LOW);
				if (totalEn < 1n) return null; // Pool is empty
				
				var index:Int; // = random.nextInt(totalEn)+1n;
				// Console.OUT.println("initial value "+index);
				var mem:Int = HIGH; 
				
				if (this.nbEntries(HIGH) > 0 && this.nbEntries(MEDIUM) > 0
						  && this.nbEntries(LOW) > 0){
					 // More probability to take the HIGH quality pool than the MEDIULM and LOW pool					 
					 val pooln = random.nextInt(10n);
					 if (pooln < 5n) // probability = 5/10 
						  mem = HIGH;
					 else if(pooln < 8) // probability = 3/10
						  mem = MEDIUM;
					 else  // probability = 2/10
						  mem = LOW;
					 
					 index = random.nextInt(nbEntries(mem)) + 1n;
				} else {	 
					 // select a random conf from all pools
					 index = random.nextInt(totalEn)+1n;
					 for(mem = 0n; index > this.nbEntries(mem); mem++){
						  index -= this.nbEntries(mem);
					 }
				}
				
				//Console.OUT.println("mem "+mem+" index "+index);
				
				//if (index >= nbEntries) Console.OUT.println("Golden: index is " + index + " needed to be < " + nbEntries);
				//if (here.id==0)Console.OUT.println(here+"alli");
				return new Maybe(pool(mem)(index-1));
		  });
	 
	 /**
	  * Get THE BEST configuration from the pool.
	  * The best configuration is always on the HIGH quality pool  
	  */
	 public def getBestConf():Maybe[CSPSharedUnit(sz)]=
		  monitor.atomicBlock(()=> {
				if (this.nbEntries(HIGH) < 1n) return null; // empty pool
				var bcost:Long = Long.MAX_VALUE;
				var best:Long = -1;
				for (i in 0n..(this.nbEntries(HIGH)-1n)){
					 if (this.pool(HIGH)(i).cost < bcost){
						  bcost = this.pool(HIGH)(i).cost;
						  best = i;
					 }
				}
				return new Maybe(pool(HIGH)(best));
		  });
	 
	 public def getCostList():String{
		  val str = new StringBuilder();
		  
		  for (j in 0..2)
				for (i in 0n..(this.nbEntries(j)-1n)){
					 str.add(this.pool(j)(i).cost);
					 str.add(" ");
				}
		  
		  return str.toString();
	 }
	 
	 /**
	  *  Clear all the entries in the pool
	  */
	 public def clear(){
		  monitor.atomicBlock(()=> {
				for (i in nbEntries.range())
					 nbEntries(i) = 0n;
				Unit()
		  });
	 }
}
public type SmartPool(s:Long) = SmartPool{self.sz==s};