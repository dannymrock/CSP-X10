package csp.model;
import csp.solver.Valuation;


public interface ICOPModel {
	 property sz():Long;
	 

	 def getMaxDomain():Int;
	 def getMinDomain():Int;
	 /**
	  *  set the random seed for the model
	  */
	 def setSeed( seed : Long):void;
	 
	 /**
	  * 	Cost on variable function (may be virtual)
	  */
	 def costOnVariable(i:Long):Long;
	 
	 /**
	  * 	Cost if move
	  */
	 def costIfMove(currentCost:Long, variable:Long, value:Long):Long;
	 
	 /**
	  * 	execute move
	  */
	 def executeMove(variable:Long, value:Long):void;
	 
	 
	  
	 def costOfSolution(shouldBeRecorded : Boolean):Long;
	 
	 //def show(s:String, d: Rail[Int]):void;
	 
	 def initialize():void;
	 
	 /**
	  * 	Default Reset function
	  * 	@param n number of variables to reset
	  * 	@param totalcost not used (for support more complex implementations)
	  * 	@return -1 for recompute cost
	  */

	 def reset ( var n : Long, totalCost : Long ) : Long ;
	 
	 def setVariables(conf : Valuation(sz)):void;
	 
	 def displaySolution(conf:Valuation(sz)):void;
	 
	 def verify(conf:Valuation(sz)):Boolean;
	 
	 def getVariables():Valuation(sz);
	 
	 // public def nextJ(i:Long, j:Long, exhaustive:Boolean) : Long {
		//   ///Console.OUT.println("i= "+i+"  j= "+j+"  bp-i= "+bpi(i));
		//   var newj:Long = j;
		//   if (j < 0 && exhaustive) // != 0n)
		// 		newj = i;
		//   
		//   return newj + 1;
	 // }
	 // 
	 // public def nextI(i:Long) : Long{
		//   return i + 1;
	 // }
	 
	 def distance(conf1 : Valuation(sz), conf2 : Valuation(sz)) : Double;
	 
}
public type ICOPModel(s:Long) = ICOPModel{self.sz==s};