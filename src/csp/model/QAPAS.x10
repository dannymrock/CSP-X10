package csp.model;
import x10.io.File;
import x10.io.FileReader;
import x10.array.Array_2;
import csp.util.Logger;
import csp.solver.Valuation;

public class QAPAS extends ModelAS 
{
	 // /** flow matrix */
	 // val f : Array_2[Long];
	 // /** distances matrix */
	 // val d : Array_2[Long];
	 
	 /** flow matrix  **/
	 val f : Rail[Rail[Int]];
	 /** distances matrix **/
	 val d : Rail[Rail[Int]];
	 
	 /** delta Matrix */
	 val delta : Array_2[Long];
	 
	 /** size of the problem (always known) */
	 //var size : Int;
	 /** optimal cost (0 if unknown) */
	 var opt : Int = 0n;
	 /** best bound (0 if unknown) */
	 var bound : Int = 0n;
	 /** best known solution cost (0 if unknown) */
	 var bks : Int = 0n;
	 ///** File */
	 //val fp  : File;
	 /** Reader */
	 //val fr : FileReader;
	 /** Region */
	 //val region : Region(1);
	 
	 //val sizeM : Long;
	 
	 def this (size :Long , seed : Long, mF:Rail[Rail[Int]], mD:Rail[Rail[Int]], 
				restLimit : Int, inV : String)
	 {
		  super(size, seed, inV);
		  
		  f = mF;
		  d = mD;
		  
		  delta = new Array_2 [Long](size, size , 0);
		  initParameters(restLimit);
	 }
	 
	 
	 // It's necessary to tune parameters
	 private def initParameters(rLimit:Int)
	 {
		  solverParams.probSelectLocMin = 60n;
		  solverParams.freezeLocMin = 5n;
		  solverParams.freezeSwap = 0n;
		  solverParams.resetLimit = 2n;
		  solverParams.resetPercent = 25n;
		  solverParams.restartLimit = rLimit;
		  solverParams.restartMax = 0n;
		  solverParams.baseValue = 0n;
		  solverParams.exhaustive = true;
		  solverParams.firstBest = true;
	 }
	 
	 /**
	  *  Compute the cost difference if elements i and j are permuted
	  */
	 public def computeDelta(i : Long, j :Long) : Long
	 {
		  var pi : Long = variables(i) as Long;
		  var pj : Long = variables(j) as Long;
		  var k : Long, pk :Long;
		  var dis : Long =
				(f(i)(i) - f(j)(j)) * (d(pj)(pj) - d(pi)(pi)) +
				(f(i)(j) - f(j)(i)) * (d(pj)(pi) - d(pi)(pj));
		  
		  for(k = 0; k < length; k++)
		  {
				if (k != i && k != j)
				{
					 pk = variables(k);
					 dis +=
						  (f(k)(i) - f(k)(j)) * (d(pk)(pj) - d(pk)(pi)) +
						  (f(i)(k) - f(j)(k)) * (d(pj)(pk) - d(pi)(pk));
				}
		  }
		  
// 		  for(k = 0; k < i; k++)
// 		  {
// 				pk = variables(k);
// 				dis +=
// 					 (f(k)(i) - f(k)(j)) * (d(pk)(pj) - d(pk)(pi)) +
// 					 (f(i)(k) - f(j)(k)) * (d(pj)(pk) - d(pi)(pk));
// 		  }
// 
// 		  while(++k < j)
// 		  {
// 				pk = variables(k);
// 				dis +=
// 					 (f(k)(i) - f(k)(j)) * (d(pk)(pj) - d(pk)(pi)) +
// 					 (f(i)(k) - f(j)(k)) * (d(pj)(pk) - d(pi)(pk));
// 		  }
// 		  while(++k < length)
// 		  {
// 				pk = variables(k);
// 				dis +=
// 					 (f(k)(i) - f(k)(j)) * (d(pk)(pj) - d(pk)(pi)) +
// 					 (f(i)(k) - f(j)(k)) * (d(pj)(pk) - d(pi)(pk));
// 		  }

		  
		  return dis;
	 }
	 
	 /**
	  *  As above, compute the cost difference if elements i and j are permuted
	  *  but the value of delta[i][j] is supposed to be known before
	  *  the transposition of elements r and s.
	  */
	 public def computeDeltaPart(i : Long, j : Long, r : Long, s : Long) : Long
	 {
		  var pi : Long = variables(i) as Long;
		  var pj : Long = variables(j) as Long;
		  var pr : Long = variables(r) as Long;
		  var ps : Long = variables(s) as Long;
		  
		  
		  
		  return (delta(i,j) +
		  (f(r)(i) - f(r)(j) + f(s)(j) - f(s)(i)) *
		  (d(ps)(pi) - d(ps)(pj) + d(pr)(pj) - d(pr)(pi)) +
		  (f(i)(r) - f(j)(r) + f(j)(s) - f(i)(s)) *
		  (d(pi)(ps) - d(pj)(ps) + d(pj)(pr) - d(pi)(pr)));
	 }
	 
	 
	 
	 public def costOfSolution(shouldBeRecorded : Boolean) : Int
	 {
		  var i : Long, j : Long;
		  var r : Long  = 0;
		  
		  for(i = 0; i < length; i++)
				for(j = 0; j < length; j++)
					 r += f(i)(j) * d(variables(i))(variables(j));
		  
		  if (shouldBeRecorded)
				for(i = 0; i < length; i++)
					 for(j = i + 1; j < length; j++)
						  delta(i,j) = computeDelta(i, j);
		  
		  return r as Int;
	 }
	 
	 
	 public def costIfSwap(currentCost : Int, i1 : Int, i2 : Int) : Int
	 {
		  return currentCost + delta(i1 as Int , i2 as Int) as Int;
	 }
	 
	 
	 public def executedSwap(var i1:Int, var i2:Int):void
	 {
		  var temp : Int = variables(i1);
	
		  if (i1 >= i2)
		  {
				var tmp : Int = i1;
				i1 = i2;
				i2 = tmp;
		  }
		  
		  var i : Long, j : Long;
		  for (i = 0; i < length; i++)
				for (j = i + 1; j < length; j++)
					 if (i != (i1 as Long) && i != (i2 as Long) && j != (i1 as Long) && j != (i2 as Long))
						  delta(i,j) = computeDeltaPart(i, j, i1, i2);
					 else
						  delta(i,j) = computeDelta(i, j);
	 }
	 
	 /** load data
	  *  load the data in filePath to the data structures matrixFlow and matrixDist 
	  *  @param filePath path of the data file to be loaded
	  *  @param mFlow flow matrix (parameter by reference)
	  *  @param mDist distance matrix  (parameter by reference)
	  *  @return true if success, false if filePath is a directory
	  */
	 static 
	 def loadData(filePath : String, mFlow:Rail[Rail[Int]], mDist:Rail[Rail[Int]]):Boolean
	 {
		  var loadTime:Long = -System.nanoTime();
		  val filep = new File(filePath);
		  if (filep.isDirectory()) return false;
		  
		  Console.OUT.println("\n--   Solving "+filePath+" ");
		 
		  //Load first line wtith headers size p1 p2
		  val fr = filep.openRead();
		  val fLine = fr.readLine(); //get first line
		  val header = readParameters(fLine);
		  
		  // for QAP:
		  // x(0) -> size
		  // x(1) -> optimum cost
		  // x(2) -> best known cost
		  val sizeF = header(0); var opt : Int = header(1); val bks = header(2);
		  var bound : Int = 0n;
		  if(opt < 0)
		  {
				bound = -opt;
				opt = 0n;
		  }
		  else
		  {
				bound = opt;
		  }
		  val vopt=opt; val vb = bound;
		  Logger.info(()=>{"file: "+filePath+" size: "+sizeF+" bound: "+vb+" opt: "+vopt+" bks: "+bks});
		  
		  //Load Problem
		  readMatrix(fr, sizeF,  mFlow, mDist);
		  fr.close();
		  return true;
	 }
	 
	 static 
	 def readParameters(line : String):Rail[Int]
	 {
		  var i : Int;
		  var j : Int = 0n;
		  var buffer:String =  "";
		  val x = new Rail[Int](3,0n); // three parameters are expected
		  for(i = 0n ; i < line.length() ; i++)
		  {
				if( line(i) == ' ' || line(i) == '\n' ) // Skip blank spaces and new line
				{ 
					 x(j++) = Int.parse(buffer);
					 //Console.OUT.println("x "+(j-1)+" = "+x(j-1));
					 buffer = "";
				}
				else
				{
					 buffer += line(i);
				}
		  }
		  x(j) = Int.parse(buffer);
		  //Console.OUT.println("x "+j+" = "+x(j));
		  
		  // for QAP:
		  // x(0) -> size
		  // x(1) -> optimum cost
		  // x(2) -> best known cost
		  
		  // if(opt < 0){
				// bound = -opt;
				// opt = 0n;
		  // }else{
				// bound = opt;
		  // }
		  
		  return x;
	 }
	 
	 static def readMatrix(fr:FileReader, sizeF:Int,  mF:Rail[Rail[Int]], mD:Rail[Rail[Int]])
	 {
		  try{
				var i : Int = 0n;
				//var charNo : Int = 0n;
				var j : Int;
				var buffer:String;
				var fLine:Int = 0n;
				var dLine:Int = 0n;
				
				for (line in fr.lines()) // It seems that the end of line characters '\n' are removed from the line
				{
					 i++;
					 buffer = ""; j = 0n;
					 if (i >= 2n && i < sizeF + 2)
					 {
						  //Console.OUT.println("mF:"+i+" :"+line);
						  // Reading Flow Matrix
						  for(char in line.chars())
						  {
								//if (char == '\n') Console.OUT.println(" new line "+buffer);
								if(char == ' ')
								{
									 if(!buffer.equals(""))
									 {
										  if (j < sizeF)
										  {
												mF(fLine)(j++) = Int.parse(buffer);
												//Console.OUT.println("mFlow "+(fLine)+","+(j-1)+" = "+(mF(fLine)(j-1)));
										  }
									 }
									 buffer = "";
								}else{
									 buffer += char;
								}                       
						  }
						  
						  // Get the last number before the end of line  
						  if(!buffer.equals("")) 
						  {
								mF(fLine)(j++) = Int.parse(buffer);
								//Console.OUT.println("mFlow "+(fLine)+","+(j-1)+" = "+(mF(fLine)(j-1)));
						  }
						  fLine++;
					 }
					 else if (i > sizeF + 2 && i <= sizeF * 2 + 2)
					 {
						  //Console.OUT.println("mD:"+i+" :"+line);
						  
						  // Reading Distance Matrix
						  for(char in line.chars())
						  {
								if( char == ' ')
								{
									 if(!buffer.equals(""))
									 {
										  if (j < sizeF)
										  {
												mD(dLine)(j++)= Int.parse(buffer);
												//Console.OUT.println("mDist "+(dLine)+","+(j-1)+" = "+(mD(dLine)(j-1)));
										  }
									 }
									 buffer = "";
								}
								else
								{
									 buffer += char;
								}                       
						  }
						  // Get the last number before the end of line  
						  if(!buffer.equals("")) 
						  {
								mD(dLine)(j++)= Int.parse(buffer);
								//Console.OUT.println("mDist "+(dLine)+","+(j-1)+" = "+(mD(dLine)(j-1)));
						  }
						  dLine++;
					 }
				}
		  }
		  catch(Exception)
		  {
				Console.OUT.println("Error reading file");
				//EOF
		  }
	 }
	 
	 /**
	  *  CHECK_SOLUTION
	  * 
	  *  Checks if the solution is valid.
	  */
	 
	 public def verify(match:Valuation(sz)) : Boolean
	 {
		  //Check Permutation
		  val permutV = new Rail[Int](sz, 0n);
		  val baseV = solverParams.baseValue;
		  for (mi in match.range())
		  {
				val value = match(mi);
				permutV(value-baseV)++;
				if (permutV(value-baseV) > 1)
				{
					 Console.OUT.println("Not valid permutation, value "+ value +" is repeted");
				}
		  }
		  
		  var i : Long, j : Long;
		  var r : Long  = 0;
		  
		  for(i = 0; i < length; i++)
				for(j = 0; j < length; j++)
					 r += f(i)(j) * d(match(i))(match(j));
		  
		  Console.OUT.println("Final cost of assignment "+r);
		  
		  return (r == 0);
	 }
}

public type QAPAS(s:Long) = QAPAS{self.sz==s};
