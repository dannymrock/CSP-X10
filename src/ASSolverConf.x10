/**	This class containts all the basic configutation info, for the
 * 	Adaptive search solver x10 implementation ASSolverPermut
 * 	
 * 	@autor Danny Munera
 * 	
 */
public class ASSolverConf {
	
	public static USE_ACTIVITIES : Int = 0; 
	public static USE_PLACES : Int = 1;
	
	/** Solver use activities or places */
	var solverMode : Int;
	/** Global Reference for communicaction between places */
	var commRef : GlobalRef[CommData];
	/** Number of itararion between each communication activity */
	var commI : Int;
	/** inter-places reset enable */
	var commEn : Int;
	
	def this( solverModeIn : Int , commR : GlobalRef[CommData], commInterval : Int , commE : Int){
		solverMode = solverModeIn;
		commRef = commR;
		commI = commInterval;
		commEn = commE;
	}
	
	public def setValues(val toSet: ASSolverConf){
		this.solverMode = toSet.solverMode;
		this.commRef = toSet.commRef;
		this.commI = toSet.commI;
	}
	
	public def communicate( totalCost : Int, csp : ModelAS ){
		if(commEn != 0){
			if(solverMode == USE_PLACES){
				/************************** Comm Places *******************************/
				//Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
				val placeid = here.id;
				at(commRef) async{
					if(commRef().isGoodCost( totalCost )){
						commRef().insertVector( totalCost , csp.variables, placeid);
					}
				}
				
				//Debug
				// if(here.id == 0){
				// 	Console.OUT.println("Print Vectors");
				// 	commRef().printVectors();
				// }
				/*********************************************************/
			}else if (solverMode == USE_ACTIVITIES){
				//Console.OUT.println("Solver Mode USE_ACTIVITIES, communication interval= "+commI);
			}else{
				Console.OUT.println("ERROR: Unknown solver mode");
			}
		}
	}
	
	
	public def getRandomVector( ){ 
		val vectorOut = (at(commRef)commRef().getVector());
		return vectorOut;
	}
	
	public def getIPVector(csp : ModelAS) : Int{
		var ret : Int = -1;
		if(commEn != 0){
			//ask for vectors in other places
			val entries = (at(commRef)commRef().nbEntries);
			if (entries < 1){
				ret = -1; //there's not avalables vectors (fail)
			}else{ 
				//get a vector
				csp.setVariables(at(commRef)commRef().getVector());
				ret = 1; 	// success
			}
		}
		return ret;
	}
}

// public def communicate(refComm : GlobalRef[CommData]){
// 	//Update shared Data

// }