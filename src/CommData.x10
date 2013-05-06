public class CommData {
	var nbEntries : Int;
	val bestPartialSolutions : Array[CSPSharedUnit](1); 
	 
	def this( ){
		nbEntries = 0;
		bestPartialSolutions = new Array[CSPSharedUnit](0..9);
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
	
	public def insertVector( cost : Int , variables : Array[Int], place:Int ) {
		var i : Int;
		//Console.OUT.println("in");
		if( nbEntries <= 9 ){
			//insert in the last place
			//Console.OUT.println("insert cost "+cost);
			//Main.show("insert vector", variables);
			bestPartialSolutions(nbEntries++) = new CSPSharedUnit( cost, variables.size , variables, place );
			//Console.OUT.println("push vector");
		}else{
			var equal : Boolean = false;
			var victim : Int = 0;
			// No place available select a victim
			for (i = 0; i < nbEntries; i++){
				if ( cost == bestPartialSolutions(i).cost ) equal = true;
				else if(cost < bestPartialSolutions(i).cost) victim =  i;
			}	
			
			if(!equal){
				bestPartialSolutions(victim) = new CSPSharedUnit( cost, variables.size , variables, place);
			}			
		}	
	}
	
	public def printVectors(){
		var i : Int;
		for (i = 0; i < nbEntries; i++){
			Console.OUT.print(i+". Cost = "+bestPartialSolutions(i).cost+" place "+bestPartialSolutions(i).place);
			Main.show(" Vector",bestPartialSolutions(i).vector);
		}
	}
	
	public def getVector():Array[Int]{
		val random = new RandomTools( 123 );
		val i = random.randomInt(nbEntries);
		return bestPartialSolutions(i).vector;
	}
	
	public def clear(){
		nbEntries = 0;
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