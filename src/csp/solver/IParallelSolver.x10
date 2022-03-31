/* 
 * COP-X10: An X10 Implementation of the CPMH framework
 * 
 * MIT License
 *
 * Copyright (c) 2022 Danny Munera
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package csp.solver;
import csp.model.GenericModel;
import csp.solver.State;
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
    def getIPVector(csp_:GenericModel(sz), myCost:Long):Boolean;
    
    def getLM(): Maybe[State(sz)];
    
    def getPR(): Maybe[State(sz)];
    
    /**
     * Send this configuration (cost, current assignment of values to variables) to
     * communication partner(s).
     */
    def communicate( info : State(sz) ):void;
    
    /**
     * Send Local Minimum configuration (cost, assignment of values to variables) to
     * communication partner(s).
     */
    def communicateLM( info : State(sz) ):void;
    
    def tryInsertLM( info : State(sz) ):void;
  
    /**
     * Insert this configuration (sent from place) into the pool P at the current place,
     * if the cost is lower than the best cost in P.
     */
    def tryInsertConf( info : State(sz) ):void;

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
            fr : Int, gr:Int, target:Boolean, fft:Int,ss:Rail[Int]{self.size==3}):void;
    def setStats(c : GlobalStats):void;

    def getConf():Maybe[State(sz)];
    def getLMConf():Maybe[State(sz)];
    
    def getBestConf():Maybe[State(sz)];

    def accStats(GlobalStats):void;
    
    def verifyWinner(ss:PlaceLocalHandle[IParallelSolver(sz)]):void;
    
    public def getCost():Long;
    def setStats_(ss:PlaceLocalHandle[IParallelSolver(sz)]):void;
    
    def solve(st:PlaceLocalHandle[IParallelSolver(sz)], cspGen:()=>GenericModel(sz), 
   			seed :Long, targetCost : Long, strictLow: Boolean ):void;
        
    def verify_(ss:PlaceLocalHandle[IParallelSolver(sz)]):void;
    	
 	//def installSolver(st:PlaceLocalHandle[IParallelSolver(sz)], solGen:()=>ISolver(sz) ):void;
    def installSolver(st:PlaceLocalHandle[IParallelSolver(sz)], solGen:(int)=>RandomSearch(sz), solverType:Int ):void;

 	
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
