package csp.solver;
 
public struct CSPSharedUnit(sz:Long, cost:Long, vector:Rail[Int]{self.size==sz}, place:Int) {}
public type CSPSharedUnit(s:Long)=CSPSharedUnit{self.sz==s};