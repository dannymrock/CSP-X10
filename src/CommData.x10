public class CommData{
	var nbEntries : Int;
	val bestPartialSolutions : Rail[CSPSharedUnit];
	val poolSize : Int;
	var bestCost : Int;
	var worstCost : Int;
	val random : RandomTools;
	 
	def this( poolS : Int ){
		nbEntries = 0;
		poolSize = poolS;
		bestPartialSolutions = new Rail[CSPSharedUnit](0..(poolSize-1));
		bestCost = Int.MAX_VALUE;
		worstCost = Int.MAX_VALUE; //
		random = new RandomTools(123L);
	}
	
	public def isGoodCost(cost : Int) : Boolean {
		var i :Int;
		if (nbEntries == 0) return true;
		for (i = 0; i < nbEntries; i++){
			if (cost <= bestPartialSolutions(i).cost)
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
			bestPartialSolutions( nbEntries++ ) = new CSPSharedUnit( cost, variables.size , variables, place );
			if (cost < bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost+" in place "+place);
			}	
			
		}else{
			// No place available select a victim
			
			var equal : Boolean = false;
			var victim : Int = 0;
			var nvic : Int = 0;
			var costToChange : Int = cost;
			
			for (i = 0; i < nbEntries; i++){
				if (worstCost == bestPartialSolutions(i).cost){
					if (random.randomInt(++nvic) == 0)
						victim = i;
				}
				
				if (cost == bestPartialSolutions(i).cost){
					if (compareVectors(variables, bestPartialSolutions(i).vector))
						return;
				}
			}	
			//Console.OUT.println("insert vector with cost "+cost);	
			bestPartialSolutions(victim) = new CSPSharedUnit( cost, variables.size , variables, place);
			
			if (cost <= bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost+" in place "+place);
			}				
		}
		updateWorstCost();
	}
	
	
	public def compareVectors (vec1 : Rail[Int], vec2 : Rail[Int]):Boolean{
		var result : Boolean = true;
		var i : Int = 0;
		for (i = 0; i < vec1.size; i++){
			if(vec1(i) != vec2(i)){
				return false;
			}
		}
		return result;
	}
	
	public def updateWorstCost(){
		var i : Int;
		var wc : Int = 0;
		for(i = 0; i < nbEntries; i++){
			if (bestPartialSolutions(i).cost > wc) wc = bestPartialSolutions(i).cost; 
		}
		worstCost = wc;	
	}
	
	public def printVectors(){
		var i : Int;
		for (i = 0; i < nbEntries; i++){
			Console.OUT.print(i+". Cost = "+bestPartialSolutions(i).cost+" place "+bestPartialSolutions(i).place);
			Main.show(" Vector",bestPartialSolutions(i).vector);
		}
	}
	
	public def getRemoteData():CSPSharedUnit{
		val random = new RandomTools( 123 );
		val i = random.randomInt(nbEntries);
		return bestPartialSolutions(i);
		
		// var i : Int;
		// var best : Int = 0;
		// for(i = 0; i < nbEntries; i++){
		// 	if (bestPartialSolutions(i).cost == bestCost) best = i; 
		// }
		// return bestPartialSolutions(best).vector;
	}
	
	public def clear(){
		nbEntries = 0;
		bestCost = Int.MAX_VALUE;
		worstCost = Int.MAX_VALUE;
	}
	
}

