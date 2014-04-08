package csp.model;

import csp.util.RandomTools;
import csp.util.Utils;
import csp.util.Logger;
import csp.solver.Valuation;

import x10.util.Random;


/** ModelAS is the Base Model implementation of a CSP problem for the Adaptive Search solver
 * 	in the X10 language.
 * 
 *  Based on the C implementation of Adaptive Search algoritm by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013
 */
public class ModelAS(sz:Long, seed:Long) {
	protected val length = sz as Int;
	protected val variables = new Rail[Int]( sz, (i:Long) => i as Int);
	protected val r  = new RandomTools(seed);
	protected val solverParams = new ASSolverParameters();
	
	/**
	 * 	Constructor of the class
	 */
	public def this( lengthProblem : Long, seed : Long ){
		property(lengthProblem, seed);
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
		solverParams.restartLimit = 1000n; // 10000000n;
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
	public def costOnVariable(i:Int):Int{
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
	public def executedSwap(i1:Int, i2:Int):void{
		//Console.OUT.println("Error executedSwap");
	}
	
	
	public def swapVariables(i:Int, j:Int):void{
		val x = variables(i);
		variables(i) = variables(j); 
		variables(j) = x;
	}
	
	public def costOfSolution(shouldBeRecorded : Boolean):Int {
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
		
		for(k in variables.range()){
			variables(k) = baseValue + k as Int;
		}
		//Main.show("before ini",variables);
		for( var i:Int = length - 1n ; i >	0n ; i-- ) {
			val j = r.randomInt( i + 1n );
			swapVariables(i,j);
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
		
		while( n-- != 0n ) {
			val i = r.randomInt(length);
			val j = r.randomInt(length);
			swapVariables(i,j);
		}
		return -1n;
	}
	
	public def setVariables(array : Rail[Int]{self.size==variables.size}){
		Rail.copy(array,this.variables);
	}
	
	public def displaySolution() {
		Utils.show("final",variables);
	}
	public def verify():Boolean=false;
	
	public def getVariables():Valuation(sz){
		return variables;
	}
	
	public def nextJ(i:Int, j:Int, exhaustive:Boolean) : Int {
		///Console.OUT.println("i= "+i+"  j= "+j+"  bp-i= "+bpi(i));
		var newj:Int = j;
		if (j < 0 && exhaustive)
			newj = i;

		return newj + 1n;
	}
}
public type ModelAS(s:Long)=ModelAS{self.sz==s};