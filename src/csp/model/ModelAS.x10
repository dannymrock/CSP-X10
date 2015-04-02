package csp.model;

import csp.util.RandomTools;
import csp.util.Utils;
import csp.util.Logger;
import csp.solver.Valuation;

import x10.util.Random;
import x10.io.FileReader;
import x10.io.File;



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
	
	val inVector:Boolean;
	val inPath:String;
	
	//Constants for Target COST
	public static val COST_FROM_COMMAND_LINE = 0;
	public static val COST_FROM_FILE_OPT = 1;
	public static val COST_FROM_FILE_BKS = 2;
	public static val STRICTLY_LOWER = -3;
	public static val LOWER_OR_EQUAL = -4;
	
	/**
	 * 	Constructor of the class
	 */
	public def this( lengthProblem : Long, seed : Long, inPath:String ){
		property(lengthProblem, seed);
		this.initParameters(1000n);
		inVector = inPath.equals(".")?false:true;
		this.inPath = inPath;
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
	private def initParameters(rLimit:Int){
		
		//Default values
		solverParams.probSelectLocMin = 0n;
		solverParams.freezeLocMin = 0n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit = length;
		solverParams.resetPercent = 10n;
		solverParams.restartLimit = rLimit;
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
		 if (inVector)
		 {
			  //initialize from inVector
			  val fileIn = new FileReader(new File(inPath));
			  val line = fileIn.readLine();
			  var i : Int;
			  var j : Int = 0n;
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
			  
			  if(j < length)
					Console.OUT.println("ModelAS ERROR: The input vector is shorter than the variables array");
			  
			  // check permutation
			  val permutV = new Rail[Int](length, 0n);
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
					variables(k) = baseValue + k as Int;
			  }
			  //Main.show("before ini",variables);
			  for( var i:Int = length - 1n ; i >	0n ; i-- )
			  {
					val j = r.randomInt( i + 1n );
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
	
	public def displaySolution(conf:Valuation(sz)) {
		Utils.show("Solution",conf);
	}
	
	public def verify(conf:Valuation(sz)):Boolean=false;
	
	public def getVariables():Valuation(sz){
		return variables;
	}
	
	public def nextJ(i:Int, j:Int, exhaustive:Int) : Int {
		///Console.OUT.println("i= "+i+"  j= "+j+"  bp-i= "+bpi(i));
		var newj:Int = j;
		if (j < 0 && exhaustive != 0n)
			newj = i;
		
		return newj + 1n;
	}
	
	public def nextI(i:Int) : Int{
		return i + 1n;
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