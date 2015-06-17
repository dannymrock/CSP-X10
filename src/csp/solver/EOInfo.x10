package csp.solver;

public class EOInfo {
	 private var firstVariable:Int = -1n;
	 private var secondVariable:Int = -1n;
	 
	 public def getFirstV():Int{
		  return firstVariable;
	 }
	 public def getSecondV():Int{
		  return secondVariable;
	 }
	  public def setFirstV(fv:Int){
		  this.firstVariable = fv;
	 }
	 public def setSecondV(sv:Int){
		  this.secondVariable = sv;
	 }	 
	 
}