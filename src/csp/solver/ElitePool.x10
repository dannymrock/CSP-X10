package csp.solver;


import csp.utils.*;

public class ElitePool{
	var nbEntries : Int;
	val pool : Rail[CSPSharedUnit];
	val poolSize : Int;
	var bestCost : Int;
	var worstCost : Int;
	val random : RandomTools;
	 
	def this( poolS : Int ){
		nbEntries = 0n;
		poolSize = poolS;
		pool = new Rail[CSPSharedUnit](poolSize);
		bestCost = Int.MAX_VALUE;
		worstCost = Int.MAX_VALUE; //
		random = new RandomTools(123L);
	}
	
	public def isGoodCost(cost : Int) : Boolean {
		var i :Int;
		if (nbEntries == 0n) return true;
		for (i = 0n; i < nbEntries; i++){
			if (cost <= pool(i).cost)
				return true;
		}
		return false;
	}
	
	public def tryInsertVector( cost : Int , variables : Rail[Int], place : Int ) {
		
		var i : Int;
	
		if (cost >= worstCost){
			return;
		}
	
		//Console.OUT.println("in");
		if( nbEntries < poolSize ){
			//insert in the last place
			//Console.OUT.println("insert cost "+cost);
			//Main.show("insert vector", variables);
			//Console.OUT.println("insert vector with cost "+cost);
			pool( nbEntries++ ) = new CSPSharedUnit( cost, variables.size as Int , variables, place );
			if (cost < bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost+" in place "+place);
			}	
			
		}else{
			// No place available select a victim
			
			var equal : Boolean = false;
			var victim : Int = 0n;
			var nvic : Int = 0n;
			var costToChange : Int = cost;
			
			for (i = 0n; i < nbEntries; i++){
				if (worstCost == pool(i).cost){
					if (random.randomInt(++nvic) == 0n)
						victim = i;
				}
				
				if (cost == pool(i).cost){
					if (compareVectors(variables, pool(i).conf))
						return;
				}
			}	
			//Console.OUT.println("insert vector with cost "+cost);	
			pool(victim) = new CSPSharedUnit( cost, variables.size as Int, variables, place);
			
			if (cost <= bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost+" in place "+place);
			}				
		}
		updateWorstCost();
	}
	
	
	public def compareVectors (vec1 : Rail[Int], vec2 : Rail[Int]):Boolean{
		var result : Boolean = true;
		var i : Int = 0n;
		for (i = 0n; i < vec1.size; i++){
			if(vec1(i) != vec2(i)){
				return false;
			}
		}
		return result;
	}
	
	public def updateWorstCost(){
		var i : Int;
		var wc : Int = 0n;
		for(i = 0n; i < nbEntries; i++){
			if (pool(i).cost > wc) wc = pool(i).cost; 
		}
		worstCost = wc;	
	}
	
	public def printVectors(){
		var i : Int;
		for (i = 0n; i < nbEntries; i++){
			Console.OUT.print(i+". Cost = "+pool(i).cost+" place "+pool(i).place);
			Utils.show(" Vector",pool(i).conf);
		}
	}
	
	public def getRemoteData():CSPSharedUnit{
		if (nbEntries < 1n){ 
			// return null; //Not possible to return null here
			return new CSPSharedUnit(Int.MAX_VALUE, 0n, null, 0n);
		}
		val index = random.randomInt(nbEntries);
		if (index >= nbEntries) Console.OUT.println("Golden: index is " + index + " needed to be < " + nbEntries);
		return pool(index);
		 
		// val random = new RandomTools( 123L );
		// val i = random.randomInt(nbEntries);
		// return pool(i);
		
		// var i : Int;
		// var best : Int = 0;
		// for(i = 0; i < nbEntries; i++){
		// 	if (bestPartialSolutions(i).cost == bestCost) best = i; 
		// }
		// return bestPartialSolutions(best).vector;
	}
	
	public def clear(){
		nbEntries = 0n;
		bestCost = Int.MAX_VALUE;
		worstCost = Int.MAX_VALUE;
	}
	
}

