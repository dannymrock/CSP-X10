/** CSPStats
 * 	This class implements a container for the CSP solver statistics. 
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 12, 2013 -> First Version
 */

public class CSPStats {
	/** Place odf the solution */
	var place : Int;
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
	 
	/**
	 *  Constructor
	 */
	def this(){
		place = -1;
		time = 0.0;
		iters = 0;
		locmin = 0;
		swaps = 0;
		reset = 0;
		same = 0;
		restart = 0;
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
	def setStats(p : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int){
		this.place = p;
		this.time = t;
		this.iters = it;
		this.locmin = loc;
		this.swaps = sw;
		this.reset = re;
		this.same = sa;
		this.restart = rs;
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
	}
	
	/**
	 * 	Print the stat values
	 * 	@param count Number of this iteration
	 */
	def print(count:Int){
		
		Console.OUT.print("| "+count+"\t| ");
		Console.OUT.printf("%.5g",time);
		Console.OUT.println("\t| "+this.iters+"\t| "+this.place+"\t| "+this.locmin+"\t| "+this.swaps+"\t| "
				+this.reset+"\t| "+(this.same/this.iters)+"\t| "+restart+"\t|");
		
	}

	/**
	 * 	Print the stat averages
	 * 	@param no total number of iterations
	 */
	def printAVG(no:Int){
		
		Console.OUT.print("| "+no+"\t| ");
		Console.OUT.printf("%.5g",time/no);
		Console.OUT.println("\t| "+this.iters/no+"\t|  N/A  | "+this.locmin/no+"\t| "+this.swaps/no+"\t| "
				+this.reset/no+"\t| "+(this.same/this.iters)+"\t| "+restart/no+"\t|");
		
	}
}