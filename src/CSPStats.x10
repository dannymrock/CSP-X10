/** CSPStats
 * 	This class implements a container for the CSP solver statistics. 
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 12, 2013 -> First Version
 */

public class CSPStats{
	/** Final Cost of solution */
	var cost : Int;	
	/** Team id solution */
	var team : Int;
	/** explorer id solution */
	var explorer : Int;
	/** time to reach the solution */
	var time : Double;
	/** Number of iterations */
	var iters : Int;
	/** Number of local minimum */
	var locmin : Int;
	/** Number of swaps */
	var swaps : Int;
	/** Number of resets */
	var reset : Int;
	/** number of same variables */
	var same : Int;
	/** number of restarts */  
	var restart : Int;
	/** Number time to change vector due to communication */ 
	var change : Int;
	 
	/**
	 *  Constructor
	 */
	def this(){
		cost = -1;
		team = -1;
		explorer = -1;
		time = 0.0;
		iters = 0;
		locmin = 0;
		swaps = 0;
		reset = 0;
		same = 0;
		restart = 0;
		change = 0; 
	}
	
	/**
	 * 	Set statistics to the object
	 * 	@param p place
	 * 	@param t time
	 * 	@param it iterations
	 * 	@param loc local minimum
	 * 	@param sw swaps
	 * 	@param re resets
	 * 	@param sa same variableplace
	 * 	@param rs restarts
	 */
	def setStats(co : Int, p : Int, e : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int){
		this.cost = co;
		this.team = p;
		this.explorer = e;
		this.time = t;
		this.iters = it;
		this.locmin = loc;
		this.swaps = sw;
		this.reset = re;
		this.same = sa;
		this.restart = rs;
		this.change = ch;
	}
	/**
	 *  Accumulate statistics in this object, Is used for average calculation
	 * 	@param stats Object with solver data to accumulate 
	 */
	def accStats(stats:CSPStats){
		this.time += stats.time;
		this.iters += stats.iters;
		this.locmin += stats.locmin;
		this.swaps += stats.swaps;
		this.reset += stats.reset;
		this.same += stats.same;
		this.restart += stats.restart;
		this.change += stats.change;
	}
	
	/**
	 * 	Print the stat values
	 * 	@param count Number of this iteration
	 */
	def print(count:Int){
		val sameIter : Float = (same as Float)/(iters as Float);
		//val changeF : Float = (change as Float)/(count as Float);
		Console.OUT.printf("| %3d | %8.4f | %8d | %2d-%2d | %8d |",count, time, iters, team, explorer, locmin);
		Console.OUT.printf(" %8d | %8d | %5.2f | %3d | %5d |\n",swaps,reset,sameIter,restart, change);
		
	}

	/**
	 * 	Print the stat averages
	 * 	@param no total number of iterations
	 */
	def printAVG(no:Int){ 
		val sameIter : Float = (same as Float)/(iters as Float);
		val changeF : Float = (change as Float)/(no as Float);
		Console.OUT.printf("| avg | %8.4f | %8d |  N/A  | %8d |",time/no, iters/no, locmin/no);
		Console.OUT.printf(" %8d | %8d | %5.2f | %3d | %5.2f |",swaps/no,reset/no,sameIter,restart/no, changeF);
		
	}
}