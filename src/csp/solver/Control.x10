package csp.solver;

import csp.utils.*;

import x10.util.Random; 

class Control{
	var nbEntries : Int;
	val pool : Rail[CSPSharedUnit] = new Rail[CSPSharedUnit] (10); // 10 max pos
	var poolSize : Int;
	var bestCost : Int;
	var worstCost : Int;
	val random : Random;
	
	var protection : Boolean;
	
	
	//Control Activity
	var event : Boolean;
	var exit : Boolean;
	var interTeam : Boolean;
	
	val monitor = new MonitorV();
	//val mPool = new MonitorV();
	
	var wait : Boolean =  false;
	

	
	def this (){
	 	nbEntries = 0n;
	 	bestCost = Int.MAX_VALUE;
	 	worstCost = Int.MAX_VALUE; //
	 	random =  new Random();
	 	//Console.OUT.println(here+"EP size constr= "+poolSize);
	 	
	 	event = false;
	 	exit = false;
	 	interTeam = false;
	 	protection = false;
	}	
	
	public def controlWait(){
		monitor.on[Unit](()=>wait, ()=>{wait = false; Unit()});
		
	}
	
	public def controlSignal(){
		wait = true;
		monitor.awaken();
		
	}
	
	
	//public def tryInsertVector( cost : Int , variables : Rail[Int], place : Int ){
		//mPool.atomicBlock[Unit](()=>insertVector(cost,variables, place));
	//}
	
	public atomic def tryInsertVector( cost : Int , variables : Rail[Int], place : Int ):Unit {
		var i : Int;
		if (cost >= worstCost)
			return Unit();
		
		if( nbEntries < poolSize ){
			pool( nbEntries++ ) = new CSPSharedUnit( cost, variables.size as Int, variables, place );
			if (cost < bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost+" in team "+place);
				//Main.show("best",variables);
			}	
			
		}else{
			// No place available select a victim
			var equal : Boolean = false;
			var victim : Int = 0n;
			var nvic : Int = 0n;
			var costToChange : Int = cost;
			for (i = 0n; i < nbEntries; i++){
				if (worstCost == pool(i).cost){
					if (random.nextInt(++nvic) == 0n)
						victim = i;
				}
				
				if (cost == pool(i).cost){
					if (compareVectors(variables, pool(i).conf))
						return Unit();
				}
			}	
			//Console.OUT.println("insert vector with cost "+cost);	
			pool(victim) = new CSPSharedUnit( cost, variables.size as Int, variables, place);
			
			if (cost <= bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost+" in team "+place);
				//Main.show("best",variables);
			}				
		}
		updateWorstCost();
		return Unit();
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
	
	public atomic def getConf():CSPSharedUnit{
		if (!protection){
			val random = new RandomTools( 123L );
			val i = random.randomInt(nbEntries);
			
			//val sol = mPool.atomicBlock[CSPSharedUnit](()=>bestPartialSolutions(i));
			
			//return sol;
			
			
			return pool(i);
		}else{
			return new CSPSharedUnit(Int.MAX_VALUE as Int, 0n, null , 0n );
		}
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
		event = false;
		exit = false;
		interTeam = false;
	}
}