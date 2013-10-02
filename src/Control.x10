import x10.util.Random; 

class Control{
	var nbEntries : Int;
	val bestPartialSolutions : Rail[CSPSharedUnit] = new Rail[CSPSharedUnit](0..10); // 10 max pos
	var poolSize : Int;
	var bestCost : Int;
	var worstCost : Int;
	val random : Random;
	
	
	//Control Activity
	var event : Boolean;
	var exit : Boolean;
	var interTeam : Boolean;
	
	var arrayRefs : Rail[GlobalRef[Team]];
	
	var minDistance : Float;
	
	def this (){
	 	nbEntries = 0;
	 	bestCost = Int.MAX_VALUE;
	 	worstCost = Int.MAX_VALUE; //
	 	random =  new Random();
	 	//Console.OUT.println(here+"EP size constr= "+poolSize);
	 	
	 	arrayRefs = new Rail[GlobalRef[Team]](0..((Place.MAX_PLACES)-1));
	 	
	 	event = false;
	 	exit = false;
	 	interTeam = false;
	 	
	 	minDistance = 0.3f;
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
					if (random.nextInt(++nvic) == 0)
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
		event = false;
		exit = false;
		interTeam = false;
	}
	
	

	public def doIterTeamComm (){// myConf : Rail[Int], myCost : Int ){
		// var nextPlace : Int = here.id + 1;
		// 
		// if (nextPlace >= Place.MAX_PLACES )
		// 	nextPlace = 0;
		
		// get conf1 from any explorer in a random team
		val place1 = random.nextInt(Place.MAX_PLACES);
		//val next = nextPlace;
		val conf1 : Rail[Int] = at(arrayRefs(place1)) arrayRefs(place1)().cspArray(0).variables;
		val cost1 : Int = at(arrayRefs(place1)) arrayRefs(place1)().solverArray(0).total_cost;
		
		// get conf2 from any explorer in a random team
		var tmp : Int = random.nextInt(Place.MAX_PLACES);
		while (place1 == tmp){
			tmp = random.nextInt(Place.MAX_PLACES);
		}
		val place2 = tmp;
		//val next = nextPlace;
		val conf2 : Rail[Int] = at(arrayRefs(place2)) arrayRefs(place2)().cspArray(0).variables;
		val cost2 : Int = at(arrayRefs(place2)) arrayRefs(place2)().solverArray(0).total_cost;
		
		// compute distance
		val dis = distance( conf1, conf2 );
		
		Console.OUT.println("distance between "+ place1+" and "+ place2+" is: " + dis);
		
		// if distance < mindistance
		if (dis < minDistance){
			// restart the team with greater cost
			if ( cost1 > cost2 ){
				//restart place1
				val x = at(arrayRefs(place1)) arrayRefs(place1)().solverArray.size; 
				for(i in 0..(x-1))
					at(arrayRefs(place1)) arrayRefs(place1)().solverArray(i).forceRestart = true;
			}else{
				//restart place2
				val x = at(arrayRefs(place2)) arrayRefs(place2)().solverArray.size; 
				for(i in 0..(x-1))
					at(arrayRefs(place2)) arrayRefs(place2)().solverArray(i).forceRestart = true;
			}
		}
	}
	
	def distance(conf1 : Rail[Int], conf2 : Rail[Int]) : Float {
		val sizeC = conf1.size;
		var i : Int = 0;
		var count : Int = 0;
		for (i = 0; i < sizeC; i++){
			//Console.OUT.println("comparing: "+conf1(i)+" - "+conf2(i));
			if(conf1(i) == conf2(i)){
				//Console.OUT.println("Equal!!!: "+conf1(i)+" - "+conf2(i));
				count++; 
			}
		}
		val dis = 1.0f - ( count as float / sizeC as float );
		return dis;
	}
	
	
}