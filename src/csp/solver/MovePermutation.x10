package csp.solver;

/**
 *  Class MovePermutation
 *  This calss models the information related to a Local Move 
 *  on a Permutation problem
 *  The first an second variables contains the index number of 
 *  the variables to swap in the variables array
 */

public class MovePermutation {
	 private var first:Int = -1n;
	 private var second:Int = -1n;
	 
	 public def this (f:Int, s:Int){
		  this.first = f;
		  this.second = s;
	 }
	 
	 public def getFirst():Int{
		  return first;
	 }
	 public def getSecond():Int{
		  return second;
	 }
	  public def setFirst(f:Int){
		  this.first = f;
	 }
	 public def setSecond(s:Int){
		  this.second = s;
	 }	 
	 
}