package csp.model;

import csp.util.Utils;
import csp.util.Logger;
import csp.solver.Valuation;

import x10.util.Random;
import x10.io.FileReader;
import x10.io.File;
import csp.solver.MovePermutation;



/** ModelAS is the Base Model implementation of a CSP problem for the Adaptive Search solver
 * 	in the X10 language.
 * 
 *  Based on the C implementation of Adaptive Search algoritm by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013
 */
public class ModelAS(sz:Long) {
	protected val size = sz;
	protected val variables = new Rail[Int]( sz, (i:Long) => i as Int);
	protected var baseValue:Int;
	protected val r:Random;
	
	//protected val solverParams = new ASSolverParameters();
	
	val inVector:Boolean;
	val inPath:String;
	val opts:ParamManager;
	// //Constants for Target COST
	// public static val COST_FROM_COMMAND_LINE = 0;
	// public static val COST_FROM_FILE_OPT = 1;
	// public static val COST_FROM_FILE_BKS = 2;
	// public static val STRICTLY_LOWER = -3;
	// public static val LOWER_OR_EQUAL = -4;
	
	/**
	 * 	Constructor of the class
	 */
	public def this( sizeProblem:Long, seed:Long, opts:ParamManager){
		property(sizeProblem);
		//this.initParameters(1000n);
		this.opts = opts;
		this.baseValue = opts("-bv", 1n);
		this.r  = new Random(seed);
		this.inPath = opts("-if","."); 
		this.inVector = inPath.equals(".") ? false : true;
	}
	
	/**
	 *  set the random seed for the model
	 */
	public def setSeed( seed : Long){
		r.setSeed(seed);
	}
	
	// /**
	//  *  Initialize the default solver parameters for the model 
	//  */
	// private def initParameters(rLimit:Int){
	// 	
	// 	//Default values
	// 	solverParams.probSelectLocMin = 0n;
	// 	solverParams.freezeLocMin = 0n;
	// 	solverParams.freezeSwap = 0n;
	// 	solverParams.resetLimit = length;
	// 	solverParams.resetPercent = 10n;
	// 	solverParams.restartLimit = rLimit;
	// 	solverParams.restartMax = 0n;
	// 	solverParams.exhaustive = false;
	// 	solverParams.firstBest = false;
	// 	solverParams.nbVarToReset = -1n;
	// }
	
	// /**
	//  * 	Set the parameter in the solver
	//  * 	@param solverParameters Solver parameter from the model
	//  */
	// public def setParameters(solverParameters : ASSolverParameters):void{
	// 	solverParameters.setValues(solverParams);
	// }
	
	/**
	 * 	Cost on variable function (may be virtual)
	 */
	public def costOnVariable(i:Long):Long{
		Console.OUT.println("Error bad costOnVariable");
		return 0;
	}
	
	/**
	 * 	Cost if swap function
	 */
	public def costIfSwap(current_cost:Long, i1:Long, i2:Long):Long{
		Console.OUT.println("Error costIfSwap");
		return 0;
	}
	
	/**
	 * 	executed swap
	 */
	public def executedSwap(i1:Long, i2:Long):void{
		Console.OUT.println("Error no executedSwap implementation");
	}
	
	
	// public def swapVariables(move:MovePermutation):void{
	// 	 val i = move.getFirst();
	// 	 val j = move.getSecond();
	public def swapVariables( i:Long, j:Long):void{
		 val x = variables(i);
		 variables(i) = variables(j); 
		 variables(j) = x;
	}
	
	public def costOfSolution(shouldBeRecorded : Boolean):Long {
		Console.OUT.println("Error costOfSolution");
		return 0;
	}
	
	static def show(s:String, d: Rail[Int]) {
		x10.io.Console.OUT.print(s + " = ");
		for(p in d.range()) 
			x10.io.Console.OUT.print(" " + d(p));
		x10.io.Console.OUT.println("");
	}
	
	public def initialize() {
		 if (inVector)
		 {
			  //initialize from inVector
			  val fileIn = new FileReader(new File(inPath));
			  val line = fileIn.readLine();
			  var i : Int;
			  var j : Long = 0;
			  var buffer:String = "";
			  
			  for(i = 0n ; i < line.length() ; i++)
			  {
					if( line(i) == ' ' || line(i) == '\n' )
					{
						 variables(j++) = Int.parse(buffer);
						 //Console.OUT.println("var "+(j-1)+" = "+variables(j-1));
						 buffer = "";
					}else
					{
						 buffer += line(i);
					}
			  }
			  if ( !buffer.equals("") )
			  {
					variables(j++) = Int.parse(buffer);
					//Console.OUT.println("var "+(j-1)+" = "+variables(j-1));
			  }
			  
			  if(j < this.size)
					Console.OUT.println("ModelAS ERROR: The input vector is shorter than the variables array");
			  
			  // check permutation
			  val permutV = new Rail[Int](this.size, 0n);
			  for (mi in variables.range())
			  {
					val value = variables(mi);
					permutV(value-1)++;
					if (permutV(value-1)>1)
					{
						 Console.OUT.println("ERROR: Not valid permutation, value "+ value +" is repeted "+mi);
					}
			  }
			  Utils.show("after ini",variables);  
		 }
		 else
		 {
			  for(k in variables.range())
			  {
					variables(k) = this.baseValue + k as Int;
			  }
			  //Main.show("before ini",variables);
			  for( var i:Long = this.size - 1 ; i > 0 ; i-- )
			  {
					val j = r.nextLong( i + 1 );
					swapVariables(i,j);
			  }
		 }
	}
	
	/**
	 * 	Default Reset function
	 * 	@param n number of variables to reset
	 * 	@param totalcost not used (for support more complex implementations)
	 * 	@return -1 for recompute cost
	 */
	public def reset ( var n : Long, totalCost : Long ) : Long {
		
		while( n-- != 0 ) {
			val i = r.nextLong(this.size);
			val j = r.nextLong(this.size);
			swapVariables(i,j);
		}
		return -1n;
	}
	
	public def setVariables(array : Rail[Int]{self.size==variables.size}){
		Rail.copy(array,this.variables);
	}
	
	public def displaySolution(conf:Valuation(sz)) {
		Utils.show("Solution",conf);
	}
	
	public def verify(conf:Valuation(sz)):Boolean=false;
	
	public def getVariables():Valuation(sz){
		return variables;
	}
	
	public def nextJ(i:Long, j:Long, exhaustive:Boolean) : Long {
		///Console.OUT.println("i= "+i+"  j= "+j+"  bp-i= "+bpi(i));
		var newj:Long = j;
		if (j < 0 && exhaustive) // != 0n)
			newj = i;
		
		return newj + 1;
	}
	
	public def nextI(i:Long) : Long{
		return i + 1;
	}
	
	
	public def distance(conf1 : Valuation(sz), conf2 : Valuation(sz)) : Double {
		var count : Int = 0n;
		for (i in 0n..(sz as Int - 1n)){
			//Logger.debug("comparing: "+conf1(i)+" - "+conf2(i));
			if(conf1(i) != conf2(i)) count++; 
		}
		val dis = count as Double / sz;
		//Console.OUT.println("distance in ModelAS = "+dis);
		return dis;
	}
}
public type ModelAS(s:Long)=ModelAS{self.sz==s};