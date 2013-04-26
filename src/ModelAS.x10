import x10.util.Random;

/** ModelAS is the Base Model implementation of a CSP problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  Based on the C implementation of Adaptive Search algoritm by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013
 */
public class ModelAS {
	var variables : Array[Int](1);
	val length : Int;
	val r : RandomTools;
	
	val varRegion : Region(1);

	val solverParams : ASSolverParameters; 
	
	/**
	 * 	Constructor of the class
	 */
	public def this( lengthProblem : Int, seed : Long ){
		varRegion = 0..(lengthProblem-1);
		solverParams = new ASSolverParameters();
		length = lengthProblem;
		variables = new Array[Int]( varRegion, ([i]:Point) => i);
		r = new RandomTools(seed);
		this.initParameters();
	}
	
	/**
	 *  set the random seed for the model
	 */
	public def setSeed( seed : Long){
		r.setSeed(seed);
	}
	
	/**
	 *  Initialize the default solver parameters for the model 
	 */
	private def initParameters(){
		
		//Default values
		solverParams.probSelectLocMin = 0;
		solverParams.freezeLocMin = 0;
		solverParams.freezeSwap = 0;
		solverParams.resetLimit = length;
		solverParams.resetPercent = 10;
		solverParams.restartLimit = 10000000;
		solverParams.restartMax = 0;
		solverParams.exhaustive = false;
		solverParams.firstBest = false;
		solverParams.nbVarToReset = -1;
	}
	
	/**
	 * 	Set the parameter in the solver
	 * 	@param solverParameters Solver parameter from the model
	 */
	public def setParameters(solverParameters : ASSolverParameters):void{
		solverParameters.setValues(solverParams);
	}
	
	/**
	 * 	Cost on variable function (may be virtual)
	 */
	public def costOnVariable(var i:Int):Int{
		//Console.OUT.println("Error bad costOnVariable");
		return 0;
	}
	
	/**
	 * 	Cost if swap function
	 */
	public def costIfSwap(current_cost:Int, i1:Int, i2:Int):Int{
		//Console.OUT.println("Error costIfSwap");
		return 0;
	}
	
	/**
	 * 	executed swap
	 */
	public def executedSwap(var i1:Int, var i2:Int):void{
		//Console.OUT.println("Error executedSwap");
	}
	
	
	public def swapVariables(val i:Int, val j:Int):void{
		val x = variables(i);
		variables(i) = variables(j); 
		variables(j) = x;
	}
		
	public def costOfSolution(shouldBeRecorded : Int):Int {
		//Console.OUT.println("Error costOfSolution");
		return 0;
	}
		
	static def show(s:String, d: Array[Int]) {
		x10.io.Console.OUT.print(s + " = ");
		for(p in d) 
			x10.io.Console.OUT.print(" " + d(p));
		x10.io.Console.OUT.println("");
	}
		
	public def initialize( baseValue : Int ) {
		var i:Int; 
		var j:Int; 
		var z:Int;
		
		for ([k] in variables) {
			variables(k) = baseValue + k;
		}
		
		for( i = length - 1 ; i >	0 ; i-- ) {
			j = r.randomInt( i + 1 );
			z = variables(i);
			variables(i) = variables(j);
			variables(j) = z;
		}
	}
	
	/**
	 * 	Default Reset function
	 * 	@param n number of variables to reset
	 * 	@param totalcost not used (for support more complex implementations)
	 * 	@return -1 for recompute cost
	 */
	public def reset ( var n : Int, totalCost : Int ) : Int {

		var i:Int;
		var j:Int;
		var x:Int;
		
		while( n-- != 0 ) {
			i = r.randomInt(length);
			j = r.randomInt(length);
			this.swapVariables(i,j);
		}
		return -1;
	}
	
	public def setVariables(array : Array[Int]){
		Array.copy(array,this.variables);
	}
	
	public def displaySolution()
	{
		Main.show("final",variables);
	}
	
}