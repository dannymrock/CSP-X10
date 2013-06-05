public class CommData{
	var nbEntries : Int;
	val bestPartialSolutions : Array[CSPSharedUnit](1);
	val poolSize : Int;
	var bestCost : Int;
	var worstCost : Int;
	 
	def this( ){
		nbEntries = 0;
		bestPartialSolutions = new Array[CSPSharedUnit](0..9);
		poolSize = 9;   
		bestCost = Int.MAX_VALUE;
		worstCost = Int.MAX_VALUE;
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
	
	public def tryInsertVector( cost : Int , variables : Array[Int], place:Int ) {	
		var i : Int;
		//Console.OUT.println("in");
		if( nbEntries <= poolSize ){
			
			//insert in the last place
			//Console.OUT.println("insert cost "+cost);
			//Main.show("insert vector", variables);
			
			bestPartialSolutions( nbEntries++ ) = new CSPSharedUnit( cost, variables.size , variables, place );
			if (cost < bestCost){ 
				bestCost = cost;
				//Console.OUT.println("New Best Cost = "+bestCost);
			}	
			updateWorstCost();
		}else{
			// No place available select a victim
			
			var equal : Boolean = false;
			var victim : Int = 0;
			
			for (i = 0; i < nbEntries; i++){
				if ( cost == bestPartialSolutions(i).cost ) equal = true;
				else if(cost < bestPartialSolutions(i).cost) victim =  i;
			}	
			
			if(!equal){
				bestPartialSolutions(victim) = new CSPSharedUnit( cost, variables.size , variables, place);
				if (cost < bestCost){ 
					bestCost = cost;
					//Console.OUT.println("New Best Cost = "+bestCost);
				}	
				updateWorstCost();
			}			
		}	
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
	
	public def getVector():Array[Int]{
		//val random = new RandomTools( 123 );
		//val i = random.randomInt(nbEntries);
		var i : Int;
		var best : Int = 0;
		for(i = 0; i < nbEntries; i++){
			if (bestPartialSolutions(i).cost == bestCost) best = i; 
		}
		return bestPartialSolutions(best).vector;
	}
	
	public def clear(){
		nbEntries = 0;
		bestCost = Int.MAX_VALUE;
		worstCost = Int.MAX_VALUE;
	}
	
}

struct CSPSharedUnit {
	val cost : Int;
	val vector : Array[Int];
	val place : Int;
	def this( costI : Int, sizeI : Int, vectorI : Array[Int], placeI : Int){
		cost = costI;
		vector = new Array[Int](0..(sizeI-1));
		Array.copy(vectorI, vector);
		place = placeI;
	}
}