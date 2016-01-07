package csp.solver;
 
public struct CSPSharedUnit(sz:Long, cost:Long, vector:Rail[Int]{self.size==sz}, place:Int,
		  solverState:Rail[Int]{self.size==3}) {}
		  //solvertype:Int ,param1:Double, param2:Int) {}
public type CSPSharedUnit(s:Long)=CSPSharedUnit{self.sz==s};