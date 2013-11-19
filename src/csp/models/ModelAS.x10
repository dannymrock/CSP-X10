package csp.models;

import csp.utils.*;

import x10.util.Random;


/** ModelAS is the Base Model implementation of a CSP problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  Based on the C implementation of Adaptive Search algoritm by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013
 */
public class ModelAS{
	public var variables : Rail[Int];
	val length : Int;
	val r : RandomTools;
	
	//val varRegion : Region(1);

	val solverParams : ASSolverParameters; 
	
	/**
	 * 	Constructor of the class
	 */
	public def this( lengthProblem : Int, seed : Long ){
		//varRegion = 0..(lengthProblem-1);
		solverParams = new ASSolverParameters();
		length = lengthProblem;
		variables = new Rail[Int]( length, (i:Long) => i as Int);
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
		solverParams.probSelectLocMin = 0n;
		solverParams.freezeLocMin = 0n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit = length;
		solverParams.resetPercent = 10n;
		solverParams.restartLimit = 10000000n;
		solverParams.restartMax = 0n;
		solverParams.exhaustive = false;
		solverParams.firstBest = false;
		solverParams.nbVarToReset = -1n;
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
		return 0n;
	}
	
	/**
	 * 	Cost if swap function
	 */
	public def costIfSwap(current_cost:Int, i1:Int, i2:Int):Int{
		Console.OUT.println("Error costIfSwap");
		return 0n;
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
		return 0n;
	}
		
	static def show(s:String, d: Rail[Int]) {
		x10.io.Console.OUT.print(s + " = ");
		for(p in d.range()) 
			x10.io.Console.OUT.print(" " + d(p));
		x10.io.Console.OUT.println("");
	}
		
	public def initialize( baseValue : Int ) {
		var i:Int; 
		var j:Int; 
		var z:Int;
		
		for(k in variables.range()){
			variables(k) = baseValue + k as Int;
		}
		//Main.show("before ini",variables);
		for( i = length - 1n ; i >	0n ; i-- ) {
			j = r.randomInt( i + 1n );
			z = variables(i);
			variables(i) = variables(j);
			variables(j) = z;
		}
		//Main.show("after ini",variables);
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
		
		while( n-- != 0n ) {
			i = r.randomInt(length);
			j = r.randomInt(length);
			this.swapVariables(i,j);
		}
		return -1n;
	}
	
	public def setVariables(array : Rail[Int]){
		Rail.copy(array,this.variables);
	}
	
	public def displaySolution()
	{
		Utils.show("final",variables);
	}
	
}