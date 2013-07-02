/**	This class containts all the basic configutation info, for the
 * 	Adaptive search solver x10 implementation ASSolverPermut
 * 	
 * 	@autor Danny Munera
 * 	
 */
public class ASSolverConf{
	
	public static USE_ACTIVITIES : Int = 0; 
	public static USE_PLACES : Int = 1;
	
	/** Solver use activities or places */
	var solverMode : Int;
	/** Global Reference for communicaction between places */
	var commRef : GlobalRef[CommData];
	/** Number of itararion between each communication activity */
	var commI : Int;
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
	
	def this( solverModeIn : Int , commR : GlobalRef[CommData], commInterval : Int , cOption : Int , ps : Int, nG : Int){
		solverMode = solverModeIn;
		commRef = commR;
		commI = commInterval;
		commOption = cOption;
		//pChange = 10;
		//refCommDist = commD ;
		poolSize = ps;
		noGroups = nG;
		myGroupId = here.id % noGroups;
		myComm = new CommData(poolSize); 
		arrayRefs = new Rail[GlobalRef[CommData]](0..((Place.MAX_PLACES)-1));
		delta = 0;
		
		//Console.OUT.println("I'm "+here.id+ " and my group is "+myGroupId);
	}
	
	public def setValues(val toSet: ASSolverConf){
		this.solverMode = toSet.solverMode;
		this.commRef = toSet.commRef;
		this.commI = toSet.commI;
	}
	/**
	 * 	communicate the vector if Searching thread totalCost is better than worstCost in the pool
	 *  @return 0 if good cost, -1 if bad cost
	 */
	public def communicate( totalCost : Int, variables : Rail[Int] ):Int{
		if(commOption != 0){
			if(solverMode == USE_PLACES){
				/************************** Comm Places *******************************/
				//Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
				val placeid = here.id;
				//val variables = csp.variables; 
				
				// All-to-one place 0
				if (commOption == 1){
					//Console.OUT.println("All-to-one");
					at(commRef) async{ commRef().tryInsertVector( totalCost , variables, placeid); }
				}else if(commOption==2){
					// All-to-All	
					//Console.OUT.println("All-to-all");
					for (k in Place.places()) if (here.id != k.id) at(arrayRefs(k.id)) 
					async {
						arrayRefs(k.id)().tryInsertVector( totalCost , variables, placeid);
					}	
				}else if (commOption == 3){ 
					//Neighbors
					//Console.OUT.println("Neighbors");
					val placeup = here.id + 1;
					val placedown = here.id - 1;
					if (placeup < Place.MAX_PLACES){
						at(arrayRefs(placeup)) async arrayRefs(placeup)().tryInsertVector( totalCost , variables, placeid);
					}
					if (placedown >= 0){
						at(arrayRefs(placedown)) async arrayRefs(placedown)().tryInsertVector( totalCost , variables, placeid);
					}
				}else if(commOption == 4){
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
		return 0;
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
		var ret : Int = -1;
		if (commOption == 1){
			// All-To-One
			//ask for vectors in other places
			val entries = (at(commRef)commRef().nbEntries);
			if (entries < 1){
				ret = -1; //there's not avalables vectors (fail)
			}else{ 
				//get a vector
				var remoteData : CSPSharedUnit = at(commRef)commRef().getRemoteData();  
				if ( (myCost + delta) > remoteData.cost ){					 
					csp.setVariables(remoteData.vector);
					ret = 1; 	// success
				}
			}
		} else if (commOption == 3){
			// Neighbors
			// Look into inner pool and get a good vector
			val myplace = here.id;
			val entries = (at(arrayRefs(myplace))arrayRefs(myplace)().nbEntries);
			if (entries < 1){
				ret = -1; //there's not avalables vectors (fail)
			}else{ 
				var localData : CSPSharedUnit = at(arrayRefs(myplace))arrayRefs(myplace)().getRemoteData();
				if ( (myCost + delta) > localData.cost ){					 
					csp.setVariables(localData.vector);
					ret = 1; 	// success
				}
			}
		}else if(commOption == 4){
			val entries = (at(arrayRefs(myGroupId))arrayRefs(myGroupId)().nbEntries);
			if (entries < 1){
				ret = -1; //there's not avalables vectors (fail)
			}else{ 
				//get a vector
				var remoteData : CSPSharedUnit = at(arrayRefs(myGroupId))arrayRefs(myGroupId)().getRemoteData();  
				if ( (myCost + delta) > remoteData.cost ){					 
					csp.setVariables(remoteData.vector);
					ret = 1; 	// success
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
		if (commOption == 1){
			at(commRef)commRef().clear();
		} else if (commOption == 3){
			at(arrayRefs(here.id))arrayRefs(here.id)().clear();
		}
	}
	
	
	public def updateVector(variables:Rail[Int], totalcost:Int){
		
		
	}
}
