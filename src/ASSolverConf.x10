/**	This class containts all the basic configutation info, for the
 * 	Adaptive search solver x10 implementation ASSolverPermut
 * 	
 * 	@autor Danny Munera
 * 	
 */
public class ASSolverConf{
	
	public static USE_ACTIVITIES : Int = 0n; 
	public static USE_PLACES : Int = 1n;
	
	/** Solver use activities or places */
	var solverMode : Int;
	/** Global Reference for communicaction between places */
	var commRef : GlobalRef[CommData];
	/** Number of itararion between each communication activity */
	var intraTI : Int;
	/** Number of itararion between each communication activity */
	var interTI : Int;
	/** inter-places reset enable */
	var commOption : Int;
	/** probability of change vector if bad cost */
	//val pChange : Int;
	
	/*** All-to-All***/
	//val refCommDist : GlobalRef[DistArray[CommData]]; 
	
	val myComm : CommData;
	
	val poolSize : Int;
	
	var arrayRefs : Rail[GlobalRef[CommData]];
	
	var delta : Int;
	
	val noGroups : Int;
	val myGroupId : Int;
	
	def this( solverModeIn : Int , commR : GlobalRef[CommData], intraTeamI : Int, interTeamI : Int , cOption : Int , ps : Int, nG : Int){
		solverMode = solverModeIn;
		commRef = commR;
		intraTI = intraTeamI;
		interTI = interTeamI;
		commOption = cOption;
		//pChange = 10;
		//refCommDist = commD ;
		poolSize = ps;
		noGroups = nG;
		myGroupId = here.id as Int % noGroups;
		myComm = new CommData(poolSize); 
		arrayRefs = new Rail[GlobalRef[CommData]](Place.MAX_PLACES);
		delta = 0n;
		
		//Console.OUT.println("I'm "+here.id+ " and my group is "+myGroupId);
	}
	
	public def setValues(val toSet: ASSolverConf){
		this.solverMode = toSet.solverMode;
		this.commRef = toSet.commRef;
		this.intraTI = toSet.intraTI;
		this.interTI = toSet.interTI;
	}
	/**
	 * 	communicate the vector if Searching thread totalCost is better than worstCost in the pool
	 *  @return 0 if good cost, -1 if bad cost
	 */
	public def communicate( totalCost : Int, variables : Rail[Int] ) : Int {
		if(commOption != 0n) {
			if(solverMode == USE_PLACES){
				/************************** Comm Places *******************************/
				//Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
				val placeid = here.id as Int;
				//val variables = csp.variables; 
				
				// All-to-one place 0
				if (commOption == 1n){
					//Console.OUT.println("All-to-one");
					at(commRef) {commRef().tryInsertVector( totalCost , variables, placeid); }
				}else if(commOption == 2n){
					// All-to-All	
					//Console.OUT.println("All-to-all");
					for (k in Place.places()) if (here.id != k.id) at(arrayRefs(k.id)) 
					async {
						arrayRefs(k.id)().tryInsertVector( totalCost , variables, placeid);
					}	
				}else if (commOption == 3n){ 
					//Neighbors
					//Console.OUT.println("Neighbors");
					val placeup = here.id + 1;
					val placedown = here.id  - 1;
					if (placeup < Place.MAX_PLACES){
						at(arrayRefs(placeup)) async arrayRefs(placeup)().tryInsertVector( totalCost , variables, placeid);
					}
					if (placedown >= 0L){
						at(arrayRefs(placedown)) async arrayRefs(placedown)().tryInsertVector( totalCost , variables, placeid);
					}
				}else if(commOption == 4n){
					at(arrayRefs(myGroupId)) async arrayRefs(myGroupId)().tryInsertVector( totalCost , variables, placeid);
				}
				
				//Debug
				 // if(here.id  == myGroupId){ //group heed
				 //   	Console.OUT.println("I'm "+myGroupId+" head group, here my pool Vectors");
				 //   	at(arrayRefs(myGroupId))arrayRefs(myGroupId)().printVectors();
				 // }
				/*********************************************************/
			}else if (solverMode == USE_ACTIVITIES){
				//Console.OUT.println("Solver Mode USE_ACTIVITIES, communication interval= "+commI);
			}else{
				Console.OUT.println("ERROR: Unknown solver mode");
			}
		}
		return 0n;
	}
	
	
	//public def getRandomVector( ) : Rail[Int]{ 
		//val vectorOut = (at(commRef)commRef().getVector());
		//return vectorOut;
	//}
	
	/**
	 *  get Inter Place Vector
	 * 
	 */
	public def getIPVector(csp : ModelAS, myCost : Int) : Int{
		var ret : Int = -1n;
		if (commOption == 1n){
			// All-To-One
			//ask for vectors in other places
			val entries = (at(commRef)commRef().nbEntries);
			if (entries < 1n){
				ret = -1n; //there's not avalables vectors (fail)
			}else{ 
				//get a vector
				var remoteData : CSPSharedUnit = at(commRef)commRef().getRemoteData();  
				if ( (myCost + delta) > remoteData.cost ){					 
					csp.setVariables(remoteData.vector);
					ret = 1n; 	// success
				}
			}
		} else if (commOption == 3n){
			// Neighbors
			// Look into inner pool and get a good vector
			val myplace = here.id;
			val entries = (at(arrayRefs(myplace))arrayRefs(myplace)().nbEntries);
			if (entries < 1n){
				ret = -1n; //there's not avalables vectors (fail)
			}else{ 
				var localData : CSPSharedUnit = at(arrayRefs(myplace))arrayRefs(myplace)().getRemoteData();
				if ( (myCost + delta) > localData.cost ){					 
					csp.setVariables(localData.vector);
					ret = 1n; 	// success
				}
			}
		}else if(commOption == 4n){
			val entries = (at(arrayRefs(myGroupId))arrayRefs(myGroupId)().nbEntries);
			if (entries < 1n){
				ret = -1n; //there's not avalables vectors (fail)
			}else{ 
				//get a vector
				var remoteData : CSPSharedUnit = at(arrayRefs(myGroupId))arrayRefs(myGroupId)().getRemoteData();  
				if ( (myCost + delta) > remoteData.cost ){					 
					csp.setVariables(remoteData.vector);
					ret = 1n; 	// success
				}
			}
		}
		return ret;
	}
	
	
	public def getWorstCostInPool():Int{
		val wCost = (at(commRef)commRef().worstCost);
		return wCost;
	}
	
	public def restartPool(){
		if (commOption == 1n){
			at(commRef)commRef().clear();
		} else if (commOption == 3n){
			at(arrayRefs(here.id))arrayRefs(here.id)().clear();
		}
	}
	
	
	public def updateVector(variables:Rail[Int], totalcost:Int){
		
		
	}
}
