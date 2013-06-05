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
	var commEn : Int;
	/** probability of change vector if bad cost */
	val pChange : Int;
	
	def this( solverModeIn : Int , commR : GlobalRef[CommData], commInterval : Int , commE : Int){
		solverMode = solverModeIn;
		commRef = commR;
		commI = commInterval;
		commEn = commE;
		pChange = 10;
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
	public def communicate( totalCost : Int, csp : ModelAS ):Int{
		if(commEn != 0){
			if(solverMode == USE_PLACES){
				/************************** Comm Places *******************************/
				//Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
				val placeid = here.id;
				if (totalCost <= getWorstCostInPool()){
					at(commRef) async{ commRef().tryInsertVector( totalCost , csp.variables, placeid); }
					return 0;
				}else{
					// try to change vector
					//Console.OUT.println("IN Conf");
					return -1;
				}
				
				
				// at(commRef) async{
				// 	val res = commRef().tryInsertVector( totalCost , csp.variables, placeid);
				// }
				// 
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
		return 0;
	}
	
	
	public def getRandomVector( ) : Array[Int]{ 
		val vectorOut = (at(commRef)commRef().getVector());
		return vectorOut;
	}
	
	/**
	 *  get Inter Place Vector
	 * 
	 */
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
	
	
	public def getWorstCostInPool():Int{
		val wCost = (at(commRef)commRef().worstCost);
		return wCost;
	}
}
