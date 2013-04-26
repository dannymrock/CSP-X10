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
	
	def this( solverModeIn : Int , commR : GlobalRef[CommData], commInterval : Int ){
		solverMode = solverModeIn;
		commRef = commR;
		commI = commInterval;
	}
	
	public def setValues(val toSet: ASSolverConf){
		this.solverMode = toSet.solverMode;
		this.commRef = toSet.commRef;
		this.commI = toSet.commI;
	}
	
	public def communicate( totalCost : Int, csp : ModelAS ){
		if(solverMode == USE_PLACES){
			/************************** Comm Places *******************************/
			//Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
			val placeid = here.id;
			at(commRef) async{
				if(commRef().isGoodCost( totalCost )){
					commRef().insertVector( totalCost , csp.variables, placeid);
				}
			}
				
			// Debug
			//if(here.id == 0){
				//Console.OUT.println("Print Vectors");
				//commRef().printVectors();
	
	//		}
			/*********************************************************/
		}else if (solverMode == USE_ACTIVITIES){
			//Console.OUT.println("Solver Mode USE_ACTIVITIES, communication interval= "+commI);
		}else{
			Console.OUT.println("ERROR: Unknown solver mode");
		}
	}
	
	
	public def getRandomVector( ){ 
		val vectorOut = (at(commRef)commRef().getVector());
		return vectorOut;
	}
}

// public def communicate(refComm : GlobalRef[CommData]){
// 	//Update shared Data

// }