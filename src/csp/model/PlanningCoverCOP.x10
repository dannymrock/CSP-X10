package csp.model;
import x10.util.Random;
import csp.solver.Valuation;
import x10.io.FileReader;
import x10.io.File;
import csp.util.Utils;

public class PlanningCoverCOP(sz:Long) implements ICOPModel{
	 property sz() = sz; //size of the problem
	 /**
	  *  Number of variables of the problem
	  *  Number of Base Stations
	  */
	 protected val size = sz;
	 /**
	  *  Array of Variables
	  */
	 protected val variables = new Rail[Int]( sz, (i:Long) => i as Int);
	 /**
	  *  Domain of the Variables
	  *  All variables in the problem use the same domain
	  *  from minDomain to max Domain
	  */
	 protected var minDomain:Int = 0n;
	 protected var maxDomain:Int = 1n;
	 /**
	  *  Random number generator
	  */
	 protected val random:Random;
	 /**
	  *  True if an initial value is provided 
	  */
	 protected val inVector:Boolean;
	 /**
	  *  Path of the file that contains the initial values of the variables 
	  */
	 val inPath:String;
	 /**
	  *  Parameters of the Program
	  */
	 val opts:ParamManager;
	 
	 
	 /** Global connectivity matrix  **/
	 val connectivity : Rail[Rail[Int]];
	 
	 val nUsers:Long;
	 
	 /**
	  *  User connectivity
	  *  each variable represent how many BS cover a given user 
	  */
	 val userCon : Rail[Int];
	 
	 
	 
	 /**
	  * 	Constructor of the class
	  */
	 public def this( sizeProblem:Long, seed:Long, opts:ParamManager){
		  property(sizeProblem);
		  this.opts = opts;
		  //this.minDomain = opts("-minD", 0n);
		  //this.maxDomain = opts("-minD", 1n);
		  this.random  = new Random(seed);
		  this.inPath = opts("-if","."); 
		  this.inVector = inPath.equals(".") ? false : true;
		  
		  
		  this.nUsers = size;
		  val users = nUsers;
		  this.userCon =  new Rail[Int](nUsers, 0n);
		  
					 
		  this.connectivity = new Rail[Rail[Int]](size, (Long) => new Rail[Int]( users, 0n ));
		  
		  for ( var i:Int = 0n; i < size; i++ ){
				Console.OUT.print( (i+1) + " : ");
				for(var j:Int = 0n; j < users; j++){
					 connectivity(i)(j) = random.nextInt(2n);
					 Console.OUT.print( connectivity(i)(j) + " ");
				}
				Console.OUT.println("");
		  } 
	 }
	
	 public def offlineUsers():Int{
		  var nonCoveredU : Int = 0n;//users without cover
		  var offlineUsers : Int = 0n;
		  var totalOfflineUsers : Int = 0n;
		  var vectsum:Int = 0n;
		  
		  for (var i:Int = 0n; i < size; i++){
				vectsum += variables(i);  
		  }
		  
		  if (vectsum > 0n){
				//The columns of the main matrix are summed to find the number of null columns
				for (var i : Int = 0n; i < nUsers; i++) { 
					 var sumc : Int = 0n;  
					 for (var j:Int = 0n; j < size; j++) {  
						  sumc += connectivity (j)(i);  
					 }
					 if (sumc < 1){
						  nonCoveredU++;
					 }
				} 

				// System.out.println();
				// System.out.println("The number of users without access is: "+NonCoveredU);
				// System.out.println(""); 
	 
				//The local offline users is calculated 
				for (var i : Int = 0n; i < nUsers; i++) { 
					 var oper:Int = 0n; //operator  
					 for (var j : Int = 0n; j < size; j++) { 
						  if( variables(j) == 1n ){
								oper += connectivity(j)(i);
						  }
					 }
					 if (oper < 1n){
						  totalOfflineUsers++;
					 }
				}  

				offlineUsers = totalOfflineUsers - nonCoveredU;
				// System.out.println(""); 
				// System.out.println("The number of offline users is: "+OfflineUsers);
		  }
		  else{
				offlineUsers = nUsers as Int;
				// System.out.println(""); 
				// System.out.println("The number of offline users is: "+OfflineUsers);
		  }
		  return offlineUsers;
	 }
	 
	 public def getMaxDomain():Int{
		  return this.maxDomain;
	 }
	 
	 public def getMinDomain():Int{
		  return this.minDomain;
	 }

	 /**
	  *  set the random seed for the model
	  */
	 public def setSeed( seed : Long):void{
		  random.setSeed(seed);
	 }
	 
	 /**
	  * 	Cost on variable function (may be virtual)
	  */
	 public def costOnVariable(i:Long):Long{
		  return 1;
	 }
	 
	 /**
	  * 	Cost if move
	  */
	 public def costIfMove(current_cost:Long, variable:Long, value:Long):Long{
		  return 1;
	 }
	 
	 /**
	  * 	execute move
	  *   On this Planning problem with only need to toggle the value of the 
	  *   variable 0->1 or 1->0 (the variable value is not used)
	  */
	 public def executeMove(varIndex:Long, value:Long) : void {
		  if (variables(varIndex) == 0n)
				variables(varIndex) = 1n;
		  else
				variables(varIndex) = 0n;
		  
		  // update values
	 }
	 
	 
	 
	 public def costOfSolution(shouldBeRecorded : Boolean):Long{
		  // for ( var i:Int = 0n; i < size; i++ ){
				// Console.OUT.print( (i+1) + " : ");
				// for(var j:Int = 0n; j < users; j++){
				// 	 connectivity(i)(j) = random.nextInt(2n);
				// 	 Console.OUT.print( connectivity(i)(j) + " ");
				// }
				// Console.OUT.println("");
		  // } 
		  for(var i:Int = 0n; i < size; i++){
				if (variables(i) == 1n){
					 for(var j:Int = 0n; j < nUsers; j++){
						  if (connectivity(i)(j) == 1n)
								userCon(j)++;
					 }
				}	 
		  }
			
		  for(var j:Int = 0n; j < nUsers; j++){
				if ( userCon(j) == 0n)
					 return 1;
		  }
		  
		  return 0;
	 }
	 
	 // public def show(s:String, d: Rail[Int]):void{
		//   
	 // }
	 
	 public def initialize():void{
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
					 variables(k) = random.nextInt(this.maxDomain + 1n) + this.minDomain;
				}
				
		  }
	 }
	 
	 /**
	  * 	default Reset function
	  * 	@param n number of variables to reset
	  * 	@param totalcost not used (for support more complex implementations)
	  * 	@return -1 for recompute cost
	  */

	 public def reset ( var n : Long, totalCost : Long ) : Long {
		  while( n-- != 0 ) {
				val i = random.nextLong(this.size);
				if(variables(i) == 0n )
					 variables(i) = 1n;
				else
					 variables(i) = 0n;
		  }
		  return -1n;
	 }
	 
	 public def setVariables(conf : Valuation(sz)):void{
		  Rail.copy(conf,this.variables);
	 }
	 
	 public def displaySolution(conf:Valuation(sz)):void{
		  Utils.show("Solution: ",conf);  
	 }
	 
	 public def verify(conf:Valuation(sz)):Boolean{
		  return false;
	 }
	 
	 public def getVariables():Valuation(sz){
		  return variables;
	 }
	 
	 public def distance(conf1 : Valuation(sz), conf2 : Valuation(sz)) : Double{
		  return 0.0;
	 }


}