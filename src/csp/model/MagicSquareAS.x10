package csp.model;
import csp.solver.Valuation;

/** MagicSquareAS is the implementation of Magic Square problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013
 * 
 * The square has dimensions this.squareLength x this.squareLength.
 */

import x10.util.Random;
import x10.array.Array_2;
public class MagicSquareAS extends GenericModel{   
	 val squareLength:Long;
	 val squareLengthm1:Long;
	 val squareLengthP1:Long;
	 var avg : Long;					/* sum to reach for each l/c/d */
	 
	 val err_l:Rail[Long];
	 val err_l_abs:Rail[Long];	  /* errors on lines (relative + absolute) */
	 val err_c:Rail[Long];
	 val err_c_abs:Rail[Long];		/* errors on columns */
	 
	 var err_d1 : Long;
	 var err_d1_abs : Long; 	/* error on d1 (\) */
	 var err_d2 : Long;
	 var err_d2_abs : Long;	/* error on d2 (/) */
	 val xref = new Rail[XRef]( sz );
	 
	 //val regionSquare : Region(1);
	 
	 /**
	  * 	Constructor
	  *  @param lengthProblem Number of variables of the problem
	  * 	@param seed Desired seed for randomness of  the problem
	  */
	 def this(sizeP:Long, seed:Long, opts:ParamManager):MagicSquareAS(sizeP){
		  super(sizeP, seed, opts);
		  this.squareLength = Math.sqrt(sizeP) as Long;
		  this.squareLengthm1 = (squareLength-1);
		  this.squareLengthP1 = (squareLength+1);
		  this.err_l     = new Rail[Long] (squareLength, 0);
		  this.err_l_abs = new Rail[Long] (squareLength, 0);	  /* errors on lines (relative + absolute) */
		  this.err_c     = new Rail[Long] (squareLength, 0);
		  this.err_c_abs = new Rail[Long] (squareLength, 0);	
		  this.avg = (squareLength * (squareLength + 1n) / 2n);		/* sum to reach for each l/c/d */
		  for( k in 0..(sizeP-1))	
				xref(k)= new XRef(squareLength, k /squareLength, k % squareLength);
		  
	 }
	 
	 // 	solverParams.probSelectLocMin = 6n;
	 // 	solverParams.freezeLocMin = 5n;
	 // 	solverParams.freezeSwap = 0n;
	 // 	//solverParams.resetLimit = 10n; //squareLength as Int / 2n;
	 // 	solverParams.resetLimit = squareLength as Int;
	 // 	solverParams.resetPercent = 10n;
	 // 	solverParams.restartLimit = rLimit;
	 // 	solverParams.restartMax = 0n;
	 // 	//solverParams.restartLimit = 2n * length;
	 // 	//solverParams.restartMax = 20n;
	 // 	solverParams.baseValue = 1n;
	 // 	solverParams.exhaustive = false;
	 // 	solverParams.firstBest = false;
	 
	 
	 /**
	  * 	costOfSolution() : Int
	  *  Compute the cost of the variables current assignation for the Magic Square problem.
	  *  @return Integer with the value of the cost of the variables current assignation.
	  */
	 public def costOfSolution(shouldBeRecorded : Boolean) : Long {
		  
		  var k:Long;
		  var r:Long;
		  var neg_avg:Long = -avg;
		  
		  //show("nuevo vector", variables);
		  
		  err_d1 = err_d2 = neg_avg;
		  
		  err_l.clear();
		  err_c.clear();
		  
		  k = 0;
		  
		  do{
				val xr  = xref(k); // is it neccessary? I can do only xref(k).get????
				//Console.OUT.println("getl "+xr.getL()+"getc "+xr.getC()+" k "+k);
				err_l(xr.l) += variables(k);
				err_c(xr.c) += variables(k);
		  }while(++k < size);
		  
		  var k1 : Long = 0;
		  var k2 : Long = 0;
		  
		  do{
				k2 += squareLengthm1;
				err_d1 += variables(k1);
				err_d2 += variables(k2);
				k1 += squareLengthP1;
		  }
		  while(k1 < size);
		  
		  // Console.OUT.println("err_d1 "+err_d1+" err_d2 "+err_d2);
		  err_d1_abs = Math.abs(err_d1);
		  err_d2_abs = Math.abs(err_d2);
		  
		  r = err_d1_abs + err_d2_abs;
		  k = 0;
		  
		  do{
				err_l(k) -= avg; 
				err_l_abs(k) = Math.abs(err_l(k)); 
				r += err_l_abs(k);
				
				err_c(k) -= avg;
				err_c_abs(k) = Math.abs(err_c(k));
				r += err_c_abs(k);
				
		  }while( ++k < this.squareLength );
		  
		  return r as Int;
	 }
	 
	 /** 
	  * 	costOnVariable( i : Int ) : Int
	  * 	This function computes the cost of individual variable i
	  * 	@param i This is the variable that we want to know the cost
	  *  @return Int value with the cost of this variable
	  */
	 public def costOnVariable( i:Long ) : Long{
		  val xr = xref(i);
		  val r = err_l_abs(xr.l) + err_c_abs(xr.c) + 
		  (xr.d1 ? err_d1_abs : 0) + (xr.d2 ? err_d2_abs : 0);
		  //r = err_l(xr.getL()) + err_c(xr.getC()) + 
		  //	(xr.d1 ? err_d1 : 0) + (xr.d2 ? err_d2 : 0);
		  
		  //r = Math.abs(r); 
		  
		  return r as Int;
	 }
	 
	 /**
	  *  costIfSwap(current_cost : Int, i1 : Int, i2 : Int) : Int
	  *  This function computes the cost of the problem if there is a swap between variable
	  *  i1 and i2.
	  * 	@param current_cost The current cost of the problem
	  *  @param i1 first variable to swap
	  *  @param i2 second variable to swap
	  *  @return cost of the problem if the swap is done
	  */
	 public def costIfSwap( current_cost:Long, i1:Long, i2:Long ) : Long {
		  
		  val xr1  = xref(i1), l1 = xr1.l, c1 = xr1.c;
		  val xr2  = xref(i2), l2 = xr2.l, c2 = xr2.c;
		  val diff1  = variables(i2) - variables(i1), diff2 = -diff1;
		  var r : Long = current_cost;
		  
		  if (l1 != l2)	{		/* not on the same line */
				r = r - err_l_abs(l1) + Math.abs(err_l(l1) + diff1); 
				r = r - err_l_abs(l2) + Math.abs(err_l(l2) + diff2);
		  }
		  
		  if (c1 != c2)	{		/* not on the same column */
				r = r - err_c_abs(c1) + Math.abs(err_c(c1) + diff1);
				r = r - err_c_abs(c2) + Math.abs(err_c(c2) + diff2);
		  }
		  
		  if (xr1.d1)	{	/* only one of both is on diagonal 1 */
				if (!xr2.d1) r = r - err_d1_abs + Math.abs(err_d1 + diff1);
		  } else if (xr2.d1) {
				r = r - err_d1_abs + Math.abs(err_d1 + diff2);
		  }
		  
		  if (xr1.d2)	{	/* only one of both is on diagonal 2 */
				if (!xr2.d2) r = r - err_d2_abs + Math.abs(err_d2 + diff1);
		  } else if (xr2.d2) {
				r = r - err_d2_abs + Math.abs(err_d2 + diff2);
		  }
		  
		  return r as Int;
	 }
	 
	 /**
	  *  executedSwap( i1 : Int, i2 : Int)
	  *  This function updates the values of the object data structures for the problem due to the 
	  *  completion of a swap between two variables
	  *  @param i1 First variable already swapped
	  *  @param i2 Second variable already swapped
	  */
	 public def executedSwap( i1:Long, i2:Long ) {
		  val xr1  = xref(i1), l1 = xr1.l, c1 = xr1.c;
		  val xr2  = xref(i2), l2 = xr2.l, c2 = xr2.c;
		  val diff1 = variables(i1) - variables(i2); /* swap already executed */
		  val diff2 = -diff1;
		  
		  
		  err_l(l1) += diff1; err_l_abs(l1) = Math.abs(err_l(l1));
		  err_l(l2) += diff2; err_l_abs(l2) = Math.abs(err_l(l2));
		  
		  err_c(c1) += diff1; err_c_abs(c1) = Math.abs(err_c(c1));
		  err_c(c2) += diff2; err_c_abs(c2) = Math.abs(err_c(c2));
		  
		  if (xr1.d1) {
				err_d1 += diff1;
				err_d1_abs = Math.abs(err_d1);
		  }
		  
		  if (xr2.d1) {
				err_d1 += diff2;
				err_d1_abs = Math.abs(err_d1);
		  }
		  
		  if (xr1.d2) {
				err_d2 += diff1;
				err_d2_abs = Math.abs(err_d2);
		  }
		  
		  if (xr2.d2) {
				err_d2 += diff2;
				err_d2_abs = Math.abs(err_d2);
		  }
	 }
	 
	 public  def verify(conf:Valuation(sz)):Boolean { //check
		  val soln = Array_2.makeView(variables, this.squareLength, this.squareLength );
		  val I = soln.numElems_1, J = soln.numElems_2;
		  
		  
		  //Check Permutation
		  val permutV = new Rail[Int](sz, 0n);
		  val baseV = baseValue;
		  for (mi in conf.range()){
				val value = conf(mi);
				permutV( value - baseV )++;
				if (permutV(value-baseV)>1){
					 Console.OUT.println("Not valid permutation, value "+ value +" is repeted");
				}
		  }
		  
		  
		  if (I != J) return false;
		  var sum:Int=0n;
		  for (j in 0..(J-1)) sum += soln(0, j);
		  val result=sum;
		  for (i in 0..(I-1)) { // row totals
				sum=0n; for (j in 0..(J-1)) sum += soln(i, j);
				if (sum != result) return false;
		  }
		  for (j in 0..(J-1)) { // col totals
				sum=0n; for (i in 0..(I-1)) sum += soln(i, j);
				if (sum != result) return false;
		  }
		  sum = 0n; for (i in 0..(I-1)) sum += soln(i,i); //diag
		  if (sum != result) return false;
		  sum = 0n; for (i in 0..(I-1)) sum += soln(i,I-1-i); //anti-diag
		  if (sum != result) return false;
		  return true;
	 }
	 /**
	  *  Xref Class
	  *  Data structure that helps to make the Magic Square funtions easier
	  */
	 static struct XRef(m:Long, l:Long,c:Long) {
		  val d1 = l==c; /* am I on the main diagonal? */
		  val d2 = (l+c)== m-1n; /* am I on the anti-diagonal? */
	 }
} //End of the Magic Square Class
public type MagicSquareAS(s:Long)=MagicSquareAS{self.sz==s};