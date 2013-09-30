class ElitePool{
	var nbEntries : Int;
	val bestPartialSolutions : Rail[CSPSharedUnit] = new Rail[CSPSharedUnit](0..10); // 10 max pos
	var poolSize : Int;
	var bestCost : Int;
	var worstCost : Int;
	val random : RandomTools;
	
	def this (){
	 	nbEntries = 0;
	 	bestCost = Int.MAX_VALUE;
	 	worstCost = Int.MAX_VALUE; //
	 	random = new RandomTools(123L);
	 	//Console.OUT.println(here+"EP size constr= "+poolSize);
	}	
	
	public atomic def tryInsertVector( cost : Int , variables : Rail[Int], place : Int ) {
		var i : Int;
		if (cost >= worstCost)
			return;
		
		if( nbEntries < poolSize ){
			bestPartialSolutions( nbEntries++ ) = new CSPSharedUnit( cost, variables.size , variables, place );
			if (cost < bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost+" in team "+place);
				//Main.show("best",variables);
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
				//Console.OUT.println("New Best Cost = "+bestCost+" in team "+place);
				//Main.show("best",variables);
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
	
	public atomic def getConf():CSPSharedUnit{
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