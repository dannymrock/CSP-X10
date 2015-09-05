package csp.solver;
import csp.model.ModelAS;
/**
 * A PARALLEL solver runs a local solver in every place, within the frame of a
 * IParallelSolver instance. All communication by a solver with other solvers
 * in the team is mediated through the frame.
 *
 * <p> Therefore to design a new parallel solver, simply have it implement
 * IParallelSolver.
 *
 */
public interface IParallelSolver {
    property sz():Long;

    /**
     * Clear whatever internal state is stored about the current problem.
     */
    def clear():void;

    /**
     * Solves the problem, which is specified by cspGen. We expect (but this is not checked in the code) that
     * all instances of the IParallelSolver frame (one in each place) is solving the same problem.
     */
   // def solve(st:PlaceLocalHandle[IParallelSolver(sz)] ):void;

    /**
     * Get some best solution from the communication partner or my local pool. If its
     * cost is less than myCost, update csp_ in place, and return true,
     * else return false.
     */
    def getIPVector(csp_:ModelAS(sz), myCost:Long):Boolean;
    def getLM(vector : Rail[Int]{self.size==sz}, myCost:Long):Boolean;

    
    /**
     * Send this configuration (cost, current assignment of values to variables) to
     * communication partner(s).
     */
    def communicate(totalCost:Long, variables:Valuation(sz)):void;
    
    /**
     * Send Local Minimum configuration (cost, assignment of values to variables) to
     * communication partner(s).
     */
    def communicateLM(totalCost:Long, variables:Valuation(sz)):void;
    
    def tryInsertLM(cost:Long, locMin:Rail[Int]{self.size==sz}, place:Int):void;
  
    /**
     * Insert this configuration (sent from place) into the pool P at the current place,
     * if the cost is lower than the best cost in P.
     */
    def tryInsertConf(cost:Long, variables:Valuation(sz), place:Int):void;

    /** Return the value of the parameter used to control communication within the team
     * (intraTeamInterval).
     *
     */

    def inTeamReportI():Int;
    def inTeamUpdateI():Int;


    /**
     * Send a signal to the associated solver to kill it. The solver will
     * kill itself the next time it checks for the kill signal.
     *
     */
    def kill():void;

    /**
     * When a place p has a solution, it invokes this method. The first place
     * to execute this method during the program run will be declared the winner;
     * for it, the method will return true. Any subsequent invocation will
     * return false.
     *
     * <p> In the invocation that returns true, kill() is invoked at every place.
     */
    def announceWinner(ss:PlaceLocalHandle[IParallelSolver(sz)], p:Long):Boolean;

    def setStats(co : Int, p : Int, e : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int,
            fr : Int, gr:Int, target:Boolean, fft:Int):void;
    def setStats(c:CSPStats):void;

    def getConf():Maybe[CSPSharedUnit(sz)];
    def getLMConf():Maybe[CSPSharedUnit(sz)];
    
    def getBestConf():Maybe[CSPSharedUnit(sz)];

    def accStats(CSPStats):void;
    
    def verifyWinner(ss:PlaceLocalHandle[IParallelSolver(sz)]):void;
    
    public def getCost():Long;
    def setStats_(ss:PlaceLocalHandle[IParallelSolver(sz)]):void;
    
    def solve(st:PlaceLocalHandle[IParallelSolver(sz)], cspGen:()=>ModelAS(sz), 
   			seed :Long, targetCost : Long, strictLow: Boolean ):void;
        
    def verify_(ss:PlaceLocalHandle[IParallelSolver(sz)]):void;
    	
 	//def installSolver(st:PlaceLocalHandle[IParallelSolver(sz)], solGen:()=>ISolver(sz) ):void;
    def installSolver(st:PlaceLocalHandle[IParallelSolver(sz)], solGen:()=>RandomSearch(sz) ):void;

 	
 	def printStats(count:Int, oF:Int, problem:Int):void;
 	def printAVG(count:Int, oF:Int, problem:Int):void;
 	def printGenAVG(count:Int, oF:Int, problem:Int):void ;
 
 	def clearSample():void;
 	def clearIntPool():void;
 	def clearDivPool():void;
 	def diversify():void;

 	def getGroupReset():Int;
 	def incGroupReset():void;
 	
}
public type IParallelSolver(s:Long)=IParallelSolver{self.sz==s};