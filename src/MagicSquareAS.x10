/** MagicSquareAS is the implementation of Magic Square problem for the Adaptive Search solver
 * 	in the x10 language.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013
 */

import x10.util.Random;
public class MagicSquareAS extends ModelAS{   
	
	var squareLength:Int; 
	
	var square_length_m1:Int;    	/* square_length - 1 */
	var square_length_p1:Int;    	/* square_length + 1 */
	var avg:Int;					/* sum to reach for each l/c/d */
	
	val err_l :Rail[Int]; 
	val err_l_abs: Rail[Int];  /* errors on lines (relative + absolute) */
	val err_c : Rail[Int];
	val err_c_abs: Rail[Int];	/* errors on columns */
	
	var err_d1:Int;
	var err_d1_abs:Int; 	/* error on d1 (\) */
	var err_d2:Int;
	var err_d2_abs:Int;	/* error on d2 (/) */
	val xref:Rail[Xref];
	
	val regionSquare : Region(1);
	
	/**
	 * 	Constructor
	 *  @param lengthProblem Number of variables of the problem
	 * 	@param seed Desired seed for randomness of  the problem
	 */
	public def this(val lengthProblem:Int, seed:Long){
		
		super(lengthProblem*lengthProblem, seed);
		squareLength = lengthProblem;
		regionSquare = 0..( squareLength - 1 );
		err_l = new Rail[Int]( regionSquare, 0 );		
		err_c = new Rail[Int]( regionSquare, 0 );		
		err_l_abs = new Rail[Int]( regionSquare, 0 );	
		err_c_abs = new Rail[Int]( regionSquare, 0 );	
		xref = new Rail[Xref]( varRegion );
		initParameters();
	}
	
	/**
	 * 	initParameters() 
	 *  Set Initial values for the problem
	 */
	private def initParameters(){
		
		val xr: Xref = new Xref();
		
		avg = squareLength * (length + 1) / 2;		/* sum to reach for each l/c/d */
		
		solverParams.probSelectLocMin = 6;
		solverParams.freezeLocMin = 5;
		solverParams.freezeSwap = 0;
		//solverParam.resetLimit = squareLength / 2;
		solverParams.resetLimit = squareLength;
		solverParams.resetPercent = 10;
		solverParams.restartLimit = 10000000;
		solverParams.restartMax = 0;
		//solverParams.restartLimit = 2 * length;
		//solverParams.restartMax = 20;
		solverParams.baseValue = 1;
		solverParams.exhaustive = false;
		solverParams.firstBest = false;
		solverParams.probChangeVector = 50; //Works with 50 and 75%
		
		square_length_m1 = squareLength - 1;
		square_length_p1 = squareLength + 1;
		
		var i : Int;
		var j : Int; 
		
		//for(var k:Int = 0; k < length; k++)
		for( [k] in varRegion)
		{	
			xref(k)= new Xref();
			
			i = k / squareLength;
			j = k % squareLength;
			
			//Console.OUT.println("i j"+i+j+(i == j)+(i + j == squareLength+1));
			xref(k).xSet(i, j, (i == j), (i + j == square_length_m1));

		}
	}
	
	/**
	 * 	costOfSolution() : Int
	 *  Compute the cost of the variables current assignation for the Magic Square problem.
	 *  @return Integer with the value of the cost of the variables current assignation.
	 */
	public def costOfSolution(shouldBeRecorded : Int) : Int {
		
		var k:Int;
		var r:Int;
		var neg_avg:Int = -avg;
		
		//show("nuevo vector", variables);

		err_d1 = err_d2 = neg_avg;
		
		err_l.clear();
		err_c.clear();
		
		k = 0;
		
		do{
			var xr:Xref = xref(k); // is it neccessary? I can do only xref(k).get????
			//Console.OUT.println("getl "+xr.getL()+"getc "+xr.getC()+" k "+k);
			err_l(xr.getL()) += variables(k);
			err_c(xr.getC()) += variables(k);
		}while( ++k < length );
		
		var k1:Int = 0;
		var k2:Int = 0;
		
		do{
			k2 += square_length_m1;
			err_d1 += variables(k1);
			err_d2 += variables(k2);
			k1 += square_length_p1;
		}
		while( k1 < length );
		
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
			
		}while( ++k < squareLength );
		
		return r;
	}
	
	/** 
	 * 	costOnVariable( i : Int ) : Int
	 * 	This function computes the cost of individual variable i
	 * 	@param i This is the variable that we want to know the cost
	 *  @return Int value with the cost of this variable
	 */
	public def costOnVariable( i : Int ) : Int{
		var xr:Xref = xref(i);
		var r:Int;

		r = err_l_abs(xr.getL()) + err_c_abs(xr.getC()) + 
			(xr.d1 ? err_d1_abs : 0) + (xr.d2 ? err_d2_abs : 0);
		//r = err_l(xr.getL()) + err_c(xr.getC()) + 
		//	(xr.d1 ? err_d1 : 0) + (xr.d2 ? err_d2 : 0);

		//r = Math.abs(r); 
		
		return r;
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
	public def costIfSwap( current_cost : Int, i1 : Int, i2 : Int ) : Int {

		var xr1:Xref = xref(i1);
		var xr2:Xref = xref(i2);
		var l1:Int = xr1.getL();
		var c1:Int = xr1.getC();
		var l2:Int = xr2.getL();
		var c2:Int = xr2.getC();
		var diff1:Int;
		var diff2:Int;
		var r:Int;
		
		r = current_cost;

		diff1 = variables(i2) - variables(i1);
		diff2 = -diff1;

		if (l1 != l2)			/* not on the same line */
		{
			r = r - err_l_abs(l1) + Math.abs(err_l(l1) + diff1); 
			r = r - err_l_abs(l2) + Math.abs(err_l(l2) + diff2);
		}
		
		if (c1 != c2)			/* not on the same column */
		{
			r = r - err_c_abs(c1) + Math.abs(err_c(c1) + diff1);
			r = r - err_c_abs(c2) + Math.abs(err_c(c2) + diff2);
		}
		
		if (xr1.d1)		/* only one of both is on diagonal 1 */
		{
			if (!xr2.d1)
				r = r - err_d1_abs   + Math.abs(err_d1   + diff1);
		}
		else if (xr2.d1)
		{
			r = r - err_d1_abs   + Math.abs(err_d1   + diff2);
		}

		if (xr1.d2)		/* only one of both is on diagonal 2 */
		{	
			if (!xr2.d2)
				r = r - err_d2_abs   + Math.abs(err_d2   + diff1);
		}
		else if (xr2.d2)
		{
			r = r - err_d2_abs   + Math.abs(err_d2   + diff2);
		}
		
		return r;
	}
	
	/**
	 *  executedSwap( i1 : Int, i2 : Int)
	 *  This function updates the values of the object data structures for the problem due to the 
	 *  completion of a swap between two variables
	 *  @param i1 First variable already swapped
	 *  @param i2 Second variable already swapped
	 */
	public def executedSwap( i1 : Int, i2 : Int) {
		var xr1:Xref = xref(i1);
		var xr2:Xref = xref(i2);
		var l1:Int = xr1.getL();
		var c1:Int = xr1.getC();
		var l2:Int = xr2.getL();
		var c2:Int = xr2.getC();
		var diff1:Int;
		var diff2:Int;
		
		diff1 = variables(i1) - variables(i2); /* swap already executed */
		diff2 = -diff1;

		err_l(l1) += diff1; err_l_abs(l1) = Math.abs(err_l(l1));
 		err_l(l2) += diff2; err_l_abs(l2) = Math.abs(err_l(l2));
 		
 		err_c(c1) += diff1; err_c_abs(c1) = Math.abs(err_c(c1));
 		err_c(c2) += diff2; err_c_abs(c2) = Math.abs(err_c(c2));
 		
 		if (xr1.d1)
 		{
 			err_d1 += diff1;
 			err_d1_abs = Math.abs(err_d1);
 		}
 
 		if (xr2.d1)
 		{
 			err_d1 += diff2;
 			err_d1_abs = Math.abs(err_d1);
 		}

 		if (xr1.d2)
 		{
 			err_d2 += diff1;
 			err_d2_abs = Math.abs(err_d2);
 		}

 		if (xr2.d2)
 		{
 			err_d2 += diff2;
 			err_d2_abs = Math.abs(err_d2);
 		}
	}
} //End of the Magic Square Class


/**
 *  Xref Class
 *  Data structure that helps to make the Magic Square funtions easier
 */
class Xref {
	var d1:Boolean;
	var d2:Boolean;
	var l:Int;
	var c:Int;
	
	public def this(){d1=false;d2=false;l=15;c=15;}
	public def xSet(line:Int, col:Int, diag1:Boolean, diag2: Boolean){
		this.l = line; this.c = col; this.d1 = diag1; this.d2 = diag2;}
	public def getL():Int{return this.l;}
	public def getC():Int{return this.c;}
}