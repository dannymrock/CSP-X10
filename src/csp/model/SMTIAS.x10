package csp.model;
import csp.util.Logger;
import csp.util.RandomTools;
import csp.solver.Valuation;
import x10.array.Array_2;
//import x10.io.File;
import x10.io.FileReader;
import x10.io.File;
import x10.io.FileWriter;
import x10.util.StringBuilder;

import x10.util.RailUtils;

public class SMTIAS extends GenericModel{
	/** menPref:Matrix with the men preferences **/
	val menPref : Rail[Rail[Int]];
	/** womenPref:Matrix with the women preferences **/
	val womenPref : Rail[Rail[Int]];
	/** revpM:Matrix with the reverse men preferences **/
	val revpM : Rail[Rail[Int]];
	/** revpW:Matrix with the men preferences **/
	val revpW : Rail[Rail[Int]];
	
	/** nbBP:Current number of blocking pairs in the configuration **/
	var nbBP:Int = 0n;
	/** nbSingles:Current number of singles in the configuration **/
	var nbSingles:Int = 0n;
	/** variablesW:array with current inverted configuration (index woman - value man) **/
	val variablesW = new Rail[Int](size,0n);
	/** errV:array with current individual cost (index man - value cost)**/
	val errV = new Rail[Int](size,-1n);
	/** bpi:Blocking par indexes (index man - value bp index to swap)**/
	val bpi = new Rail[Int](size,-1n);
	/** sgV : vector with singles men in match **/
	val sgV = new Rail[Long](size,-1);
	
	/** weight: weight to compute the total cost (cost = bp*weight + singles)**/
	var weight:Long = size;
	
	var singlei:Int = 0n;
	
	
	var minSingles:Int = 0n;
	
	val isHRT:Boolean;
	
	val mapTable:Rail[Int];
	
	val mprefsize = new Rail[Int](size, 0n);
	
	// val problemFile:String;
	
	// static val NO_FILE = "no_file_provided";
	
	public def this (sizeProblem : Long , seed : Long, opts:ParamManager, isHRT:Boolean,
			  mPref:Rail[Rail[Int]], wPref:Rail[Rail[Int]], mTable:Rail[Int]) : SMTIAS(sizeProblem){
		super( sizeProblem, seed, opts);
		
		val l = size as Int;
		
		// this.problemFile = opts("-f", NO_FILE);
		// if (problemFile.equals(NO_FILE))
		// 	 Console.OUT.println("ERROR: no problem file provided in QAP");
		
		// TODO: Load from file size here
		
		//menPref = new Rail[Rail[Int]](size, (Long) => new Rail[Int]( size, 0n ));
		//womenPref = new Rail[Rail[Int]](size, (Long) => new Rail[Int]( size, 0n ));
		menPref = mPref;
		womenPref = wPref;
		
		// this.mapTable = new Rail[Int](size, 0n);
		this.mapTable = mTable;
		this.isHRT = isHRT;
			
		revpM = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		revpW = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		// Creating Reverse Matrixes
		var mw:Int,pos:Int;
		var level:Int=0n;
		for(mw = 0n; mw < size; mw++){
			level = 0n;
			var man:Int;
			for(pos=0n; ( man = womenPref(mw)(pos)) != 0n; pos++ ){
				if (man > 0n) 
					level++;
				else // if current value is negative = tie, same level as the previous one
					man = -man;
				
				//Convertion to an index	
				revpW(mw)(man - 1) = level;
			}
		}
		
		for(mw = 0n; mw < size; mw++){
			level=0n;
			var woman:Int;
			for(pos = 0n; (woman = menPref(mw)(pos)) != 0n; pos++ ){
				if (woman > 0n) 
					level++;
				else // if current value is negative = tie, same level as the previous one
					woman =-woman;
				
				//Converting to an index	
				revpM(mw)(woman - 1) = level;
			}
			mprefsize(mw) = pos;
			//Console.OUT.println("size "+mw+" = "+pos);
		}
		//printPreferencesTables();
		//writeSMTIFile("outSMTI.smp");
	}
	
	// /** initParameters
	//  *  It is necessary to fine tune the parameters for this problem
	//  */
	// private def initParameters(rLimit:Int){
	// 	
	// 	solverParams.probSelectLocMin =  100n; // try ~80%  
	// 	solverParams.freezeLocMin = 1n;
	// 	solverParams.freezeSwap = 0n;
	// 	solverParams.resetLimit =1n; 
	// 	solverParams.resetPercent = 0n;
	// 	solverParams.restartLimit = rLimit;/*30n*size*/; 
	// 	solverParams.restartMax = 0n;
	// 	solverParams.baseValue = 1n;
	// 	solverParams.exhaustive = false;
	// 	solverParams.firstBest = true;
	// 	
	// 	//solverParams.probChangeVector = probCV; //10n;
	// 	
	// 	//weight = System.getenv().get("V")!=null?size:1n;
	// 	//Console.OUT.println("restart Limit= "+solverParams.restartLimit);
	// 	//Console.OUT.println("Prob Change Vector= "+solverParams.probChangeVector);
	// 	
	// }
	
	
	/**
	 * Determine if mi and wi is a BP return the error (or 0 if it's not a BP)  
	 * @param wi index of the woman 
	 * @param pwi index of current match of the woman
	 * @param mi index of the man
	 * @return the error if (mi,wi) is a BP (0 else)
	 */
	public def blockingPairError(wi:Int, pwi:Int, mi:Int) : Int {
		var err:Int ;
		val lvC = revpW(wi)(pwi);
		val lvD = revpW(wi)(mi);     
		if (lvD == 0n){
			// m is not present in the preferences of w
			err = 0n;
		} else if (lvC == 0n){   //current assignment of w (pw) is invalid, not present in Wprefs 
			err = 1n;
		}else  {
			err = lvC - lvD;	// computing distance between current and desired assignment
			if (err > 0) {      // if err > 0 (m,w) is a BP (blocking pair) 
				err++;          // Penalize BP over singles (err *= 2n; also works)
			} else { 
				err = 0n;
			}		
		}	
		
		//Console.OUT.println("bpE in w "+w+", pw "+pw+", m "+m +" = "+err);
		return err;
	}
	
	
	/**
	 * 	Computes the cost of the solution
	 * 	count the number of blocking pairs and singles for the current Match
	 *  if shouldBeRecorded is true the global variables are updated
	 *  @param shouldBeRecorded if true saves the computatuon in global variables
	 * 	@return cost
	 */
	public def costOfSolution( shouldBeRecorded : Boolean ) : Long {	
		 var w : Int;
		 var pmi : Int = 0n; // index of current match of mi 
		 var pwi : Int = 0n; // index of current match of wi
		 var bpnumber : Int = 0n;
		 var singles : Int = 0n;
		 var flagBP : Boolean = false; // true when the first undominated BP is found
		 
		 variablesW.clear();
		 for (mi in variables.range()) // mi -> man index (man number = mi + 1)
		 { 
			  variablesW( variables(mi) - 1 ) = mi as Int + 1n;
		 }
		 
		 // verify existence of undomminated BP's for each man 
		 for ( mi in variables.range() ) // mi -> man index (man number = mi + 1)
		 {  
			  pmi = variables(mi) - 1n; // pm current match of mi (if pm is not valid, mi is single)
			  
			  var bpMi : Int = -1n;		//NB: -1 to avoid false positive in the test of costIfSwap
			  var uBPn : Int = 0n;      // number of undominated BP per man
			  var e : Int = 0n; 	 	
			  var levelPM : Int = revpM(mi)(pmi); // m's current match level of preference  
			  
			  if(levelPM == 0n )   //verify if m is single (pm is not a valid match)
			  { 
					levelPM = size as Int + 1n;  // FIX (single should be size +1 )
					singles++;
					
					// if (shouldBeRecorded && r.randomInt(singles) == 0n)
					// {
					// 	 singlei = mi as Int;
					// }
					
					if (shouldBeRecorded)
						 sgV(singles-1) = mi;
			  }
			
			  var levelW : Int = 0n;
			  var li : Long=0;
			  for( li = 0; ( w = menPref(mi)(li) ) != 0n; li++ )
			  {
					if ( w > 0n )
					{
						 levelW++;			// new level of preference
						 if (flagBP)
						 {
							  bpnumber++; // only count the men involved in BP not the number of BPs
							  break;  // Only consider undominated BP
						 }
					}
					else
					{
						 w = -w;             // if w < 0 -> same level of preference (tie), "restore" w
					}
					
					if (levelW >= levelPM) // stop if cuerrent level of pref is bigger or equal 
						 break;             // than the level of pref of pm (current match) "stop condition"  
					
					pwi = variablesW(w-1)-1n; // pwi index of the current match of the woman w
					
					val cError = blockingPairError(w-1n, pwi, mi as int);  // check if w prefers m to pw 
					if (cError > 0n)
					{	
						 // only consider undominated BP
						 if ( !flagBP ) // first uBP
						 {
							  //Console.OUT.println("first uBP ("+(mi+1)+","+w+")");
							  flagBP = true;
							  bpMi = pwi;
							  e = cError;     
							  uBPn = 1n;  // count the errors (number of undominated BP)
							  
							  // if (r.randomDouble() < 0.0) 
							  // if (r.randomDouble() <= 0.98)  //prob to only select the first uBP	
							  // {
							  bpnumber++;
							  break;
							  // }
						 } 
						 else
						 {
							  //Console.OUT.println("other uBP ("+(mi+1)+","+w+")");
							  // if (r.randomInt(++uBPn) == 0n)  // random selection if there are more than one uBP
							  // {
							  //   e = cError;
							  //   bpMi = pwi;
							  // }
							  
							  if ( cError > e )  // select the larger error
							  {
									e = cError;
									bpMi = pwi;
							  }
							  
							  
							  // e = cError; // the last one :S
							  // bpMi = pwi;
							  
							  // if (uBPn > 2)
							  // {
								//	bpnumber++;
								//	break;
							  // }
							  
							  // if (uBPn++ > 5n)
							  // {
									// bpnumber++;
									// break;
							  // }	
							  
						 }
					}
			  }
			  
			  if ( shouldBeRecorded )
			  {
					errV(mi) = e;
					bpi(mi) = bpMi;
					//Console.OUT.println("mi= "+mi+" e= "+e+" bpMi= "+bpMi+ " in total this man have "+ uBPn+" undominated bps");
			  }
			  
			  // clean variables for next man 
			  flagBP = false;
			  uBPn = 0n;
		 }
		 if (shouldBeRecorded) 
		 {
			  nbBP = bpnumber;
			  nbSingles = singles;
			  ///Console.OUT.println("totalCost= "+(bpnumber*weight+singles));
		 }
		 
		 //         val s2 : Int = 2n * singles - size;  // check the case where singles is an odd number, is it correct to use round up, ie. (singles +1) / 2 ?
		 // 
		 //         if (bpnumber == 0n ) // check if the mariage is stable and if it improves the min number of singles
		 //         {
		 //         	Console.OUT.println("minSingles= "+minSingles+" Singles= "+singles);
		 //         	}
		 //         
		 //         if (bpnumber == 0n && s2 > minSingles) // check if the mariage is stable and if it improves the min number of singles
		 //         	{
		 //         	Console.OUT.println("minSingles= "+minSingles+" Singles= "+singles+" New min singles= "+s2);	
		 //         	minSingles = s2;
		 //         
		 //         	}
		 // 
		 //         // DEBUG: this test is for debug only - should never be true
		 //         if (singles < minSingles)
		 //         	Console.OUT.println("ERROR "+singles+" <-> "+minSingles);
		 // 
		 //         return bpnumber * weight + singles - minSingles; // take into account the best min_singles known so far
		 
		 
		 return bpnumber * weight + singles;	
	}
	
	/** 
	 * 	costOnVariable( i : Int ) : Int
	 * 	This function computes the cost of individual variable i
	 *  For SMP returns the distance in the blocking pair
	 * 	@param i This is the variable that we want to know the cost
	 *  @return Int value with the cost of this variable
	 */
	public def costOnVariable( i : Long ) : Long {		
		return errV(i);
	}
	
	
	public def nextJ(i:Long, j:Long, exhaustive:Boolean) : Long {
		///Console.OUT.println("i= "+i+"  j= "+j+"  bp-i= "+bpi(i));
		return (j < 0n) ? bpi(i) : -1n;
	}
	
	
	/**
	 *  executedSwap( i1 : Int, i2 : Int)
	 *  This function updates the values of the object data structures for the problem due to the 
	 *  completion of a swap between two variables
	 *  @param i1 First variable already swapped
	 *  @param i2 Second variable already swapped
	 */
	public def executedSwap ( i1:Long, i2:Long ) {
		this.costOfSolution(true);
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
	public def costIfSwap(current_cost:Long, var i1:Long, var i2:Long) : Long {
		swapVariables(i1, i2);
		var r : Long = costOfSolution(false);
		swapVariables(i1, i2);
		return r;
	}
	
	
	public def findMax(pvalue1:Int, pvalue2:Int):Int { //pvalue is an man index
		var maxi:Int = -1n;		/* return -1 if none found */
		var maxErr:Int = 0n;
		var maxNb:Int = 0n;
		var i:Int;
		
		for(i = 0n; i < size; i++){
			var e:Int = errV(i);
			if (e <= 0n || i == pvalue1 || i == pvalue2 ||  bpi(i) == pvalue1 || bpi(i) == pvalue2) 
				continue;
			if (e > maxErr){ 	
				maxi = i;
				maxErr = e;
				maxNb = 1n;
			} 
			else if (e == maxErr && r.nextInt(++maxNb) == 0n){
				maxi = i;
			}
		}
		// val mi = maxi;
		// Logger.debug(()=>"maxi:"+mi);
		return maxi;
	}
	
	public def reset ( var n:Long , totalCost : Long ) : Long {			
		
		// 1st BLOCKING PAIRS
		if (nbBP > 0n){
			var maxi:Int = findMax(-1n, -1n);	/* find max */
			var bpiMaxi:Int = bpi(maxi);
			var otheri:Int; 
			///Console.OUT.println("Reset maxi= "+maxi+" bpiMaxi = "+ bpiMaxi);
			swapVariables(maxi, bpiMaxi);
			if (nbBP > 1n && r.nextDouble() < 0.98 &&  (otheri = findMax(maxi,bpiMaxi)) >= 0n){
				///Console.OUT.println("Reset otheri= "+otheri+" bpi(otheri) = "+ bpi(otheri));
				swapVariables(otheri, bpi(otheri));
				return -1n;
			}
		}
		//else
		if (nbSingles > 0) {
			 
			 // * Assign a random partner to a single
			 //val j = r.randomInt(size);
			 ///Console.OUT.println("Reset single singV("+singlei+")= "+singV(singlei)+" random j = "+ j+"  nbSingles="+nbSingles);
			 //swapVariables(singlei, j);		
			 
			 // * Assign the best partner to a single
			 
			 // search the man married with the "best" partner of singlei
			 //var bestpar : Int = menPref(singlei)(0)  ;
			 
			 // val j = r.randomInt( mprefsize(singlei) ); 
			 // val bestpar = Math.abs( menPref(singlei)(j) );			 
			 // val manToSwap = variablesW( bestpar - 1 ) - 1n ;
			 // //Console.OUT.println("Reset single singV("+singlei+"),best par "+ bestpar +"  best = "+ manToSwap+"  nbSingles="+nbSingles);
			 // swapVariables(singlei, manToSwap);	
			 
			 
			 // select a single man
			 val singleman = sgV( r.nextInt( nbSingles ) );
			 // select a woman in the pref list of the single
			 val j = r.nextInt( mprefsize(singleman) ); 
			 val bestpar = Math.abs( menPref(singleman)(j) );
			 // find man paired with bestpar woman
			 val manToSwap = variablesW( bestpar - 1 ) - 1n ;
			 //Console.OUT.println("Reset single singV("+singlei+"),best par "+ bestpar +"  best = "+ manToSwap+"  nbSingles="+nbSingles);
			 swapVariables(singleman as Int, manToSwap);		
			 
			 
			 // Try to match a second single t
			 // if (nbSingles > 1 && r.randomDouble() < 0.98)
			 // {	  
				//   // select a second single man
				//   val singleman2 = sgV( r.randomInt( nbSingles ) );
				//   // select a woman in the pref list of the single
				//   val wj = r.randomInt( mprefsize(singleman2) ); 
				//   val bestpar2 = Math.abs( menPref(singleman2)(wj) );
				//   // find man paired with bestpar woman
				//   val manToSwap2 = variablesW( bestpar2 - 1 ) - 1n ;
				//   
				//   //Console.OUT.println("Reset single singV("+singlei+"),best par "+ bestpar +"  best = "+ manToSwap+"  nbSingles="+nbSingles);
				//   swapVariables(singleman2 as Int, manToSwap2);	
			 // }
		} else {
			val i = r.nextLong(size);
			val j = r.nextLong(size);
			///Console.OUT.println("Reset no 2nd BP i= "+i+" j = "+ j);
			swapVariables(i, j);
		}
		return -1n;
		
	}
	
	/**
	 *  CHECK_SOLUTION
	 * 
	 *  Checks if the solution is valid.
	 */
	
	public def verify(match:Valuation(sz)):Boolean
	{
		var w:Int;
		var pmi:Int = 0n;
		var pwi:Int = 0n; //w_of_m, m_of_w;
		var r:Int = 0n;
		var singles:Int = 0n;
		
		val permutV = new Rail[Int](sz, 0n);
		val variablesWv = new Rail[Int](size,0n);
		for (mi in match.range())
		{
			val value = match(mi);
			permutV(value-1)++;
			if (permutV(value-1)>1)
			{
				Console.OUT.println("ERROR: Not valid permutation, value "+ value +" is repeted");
			}
			if (value==0n)
				 Console.OUT.println("ERROR: not valid Zero in solution");
			variablesWv(value-1) = mi as Int + 1n;
		}
		
		// verify existence of undomminated BP's for each man 
		for (mi in match.range())  // mi -> man index (man number = mi + 1)
		{  
			pmi = match(mi)-1n; // pm current match of mi 
			var e:Int = 0n; 	 	
			var bF:Int = 0n;
			var levelPM:Int = -1n; //m's current match level of preference  
			
			if( revpM(mi)(pmi)==0n )
			{
				levelPM = size as Int; //put some value
				singles++;
				// Console.OUT.println("m "+ (mi+1n) +" is SINGLE (not a valid match with w "+(pmi+1n)+")");
				val hos = mapTable(pmi);
				//Logger.info(()=>{"r "+ (mi+1n) +" is SINGLE (not a valid match with h "+(hos+1n)+")"});
			} 
			else
			{ // m has a valid assignment pm
				levelPM = revpM(mi)(pmi);
			}
			
			var levelW:Int = 0n;
			for(li in menPref(mi).range()) //li level of preference index
			{ 
				w = menPref(mi)(li);
				if (w == 0n)
					 continue;	// entry deleted
				
				if(w > 0n)			// new level of preference
					levelW++;
				else						// if w < 0 -> same level of preference (tie) 
					w = -w;
				
				if (levelW >= levelPM)
					 break; //stop if cuerrent level of pref is bigger or equal 
				
				// than the level of pref of pm (current match) "stop condition"
				pwi = variablesWv(w-1)-1n; //pw current match of the current
				// 	// Verify if w prefers m to pw
				e = verifyBlockingPairError(w-1n, pwi, mi as Int);
				
				if (e > 0n)
				{
					r++;
					val wval=w; val pwval=pwi+1n; val eval=e;
					Logger.debug(()=>{"ERROR: blocking pair m="+(mi+1n)+" w="+wval+" pw= "+pwval +" with error= "+eval});
					/* count the errors (number of BP) */
					break; 			//only consider undominated BP
				}
			}
		}
		return (r + singles == 0n);
	}
	
	public def verifyBlockingPairError(wi:Int, pwi:Int, mi:Int) : Int {
		var errv:Int ;
		val lvCv = revpW(wi)(pwi);
		val lvDv = revpW(wi)(mi);     
		if (lvDv == 0n){
			// m is not present in the preferences of w
			errv = 0n;
		} else if (lvCv == 0n){   //current assignment of w (pw) is invalid, not present in Wprefs 
			errv = 1n;
		}else  {
			errv = lvCv - lvDv;	// computing distance between current and desired assignment
			if (errv > 0) {      // if err > 0 (m,w) is a BP (blocking pair) 
				errv++;          // Penalize BP over singles (err *= 2n; also works)
			} else { 
				errv = 0n;
			}		
		}	
		return errv;
	}
	
	// /**
	//  * 	Set the parameter in the solver
	//  * 	@param solverParameters Solver parameter from the model
	//  */
	// public def setParameters(solverParameters : ASSolverParameters):void{
	// 	solverParameters.setValues(solverParams);
	// }
	
	// public def initialize( baseValue : Int ) {
	// 	for(k in variables.range()){
	// 		variables(k) = baseValue + k as Int;
	// 	}
	// 	//Main.show("before ini",variables);
	// 	for( var i:Int = size - 1n ; i >	0n ; i-- ) {
	// 		val j = r.randomInt( i + 1n );
	// 		swapVariables(i,j);
	// 	}
	// }
	
	// public def swapVariables( i:Long, j:Long):void{
	// 	//Console.OUT.println("swap func i: "+i+" j: "+j);
	// 	val x = variables(i);
	// 	variables(i) = variables(j); 
	// 	variables(j) = x;
	// }
	
	// public def setVariables(array : Rail[Int]{self.size==variables.size}){
	// 	Rail.copy(array,this.variables);
	// }
	// 
	// public def getVariables():Valuation(sz){
	// 	return variables;
	// }
	
	// public def getnbSingles():Int{
	// 	return nbSingles;
	// }
	// 
	// public def getnbBP():Int{
	// 	return nbBP;
	// }
	
	public def displaySolution(){
		Console.OUT.println("\nMatching  m->w:");
		for (i in variables.range()){
			Console.OUT.printf("%4d->%-4d",(i+1),variables(i));
		}
		Console.OUT.print("\n");
	}
	
	public def displaySolution(match:Valuation(sz)){	
		 if(isHRT)
		 {
			  Console.OUT.println("\nMatching  r->h:");
			  for (i in match.range())
			  {
					if(revpM(i)(match(i)-1n)==0n)
					{
						 Console.OUT.printf("%4d->%-4d",(i+1),0n);
					}else
					{
						 val hos = mapTable(match(i)-1);
						 Console.OUT.printf("%4d->%-4d",(i+1),(hos+1));
					}
			  }
			  Console.OUT.print("\n");
		 }
		 
		 //else 
		 {
			  //Logger.debug(()=>{"\n SMTI Solution Vector:"});
			  Console.OUT.print("SMTI Solution Vector: ");
			  for (i in match.range())
			  {
					if(revpM(i)(match(i)-1n) == 0n)
						 Console.OUT.print(0+" ");
					else
						 Console.OUT.print(match(i)+" ");
			  }
			  Logger.debug(()=>{"\n"});
			  
			  // Console.OUT.println("\nMatching  m->w:");
			  // for (i in match.range())
			  // {
					// if(revpM(i)(match(i)-1n)==0n)
					// {
					// 	 Console.OUT.printf("%4d->%-4d",(i+1),0n);
					// }
					// else
					// 	 Console.OUT.printf("%4d->%-4d",(i+1),match(i));
			  // }
			  // Console.OUT.print("\n");
		 }
	}
	
	public def displaySolution2 (match:Valuation(sz)){	
		Console.OUT.print("#Sol in  "+here);
		for (i in match.range()){
			if(revpM(i)(match(i)-1n)==0n){
				Console.OUT.print(" - ");
			}else
				Console.OUT.print(" "+variables(i)+" ");
		}
		Console.OUT.print("\n");
	}
	
	private def writeSMTIFile(fileName:String){
		 
		 //Write file
		 val oFile = new File(fileName);
		 val p = oFile.printer();
		 p.println(size);		
		 p.println(" ");		
		 
		 
		 var i:Int = 0n;
		 for (i=0n; i<size; i++){
			  //Console.OUT.print(i+1+": ");
			  for(j in menPref(i))
					if (j != 0n) p.print(j+" ");
			  p.println("");
		 }
		 p.println(" ");
		 for (i=0n; i<size; i++){
			  for(j in womenPref(i))
					if (j != 0n) p.print(j+" ");
			  p.println("");
		 }
	}
		 
	private def printPreferencesTables(){
		 
		 Console.OUT.println("\nMen Preferences");
		 var i:Int = 0n;
		 for (i=0n; i<size; i++){
			  Console.OUT.print(i+1+": ");
			  for(j in menPref(i))
					Console.OUT.print(j+" ");
			  Console.OUT.println("");
		 }
		 Console.OUT.println("Women Preferences");
		 for (i=0n; i<size; i++){
			  Console.OUT.print(i+1+": ");
			  for(j in womenPref(i))
					Console.OUT.print(j+" ");
			  Console.OUT.println("");
		  }
		
		// Console.OUT.println("Men rev Preferences");
		// for (i=0n; i<size; i++){
		// 	Console.OUT.print(i+1+": ");
		// 	for(j in revpM(i))
		// 		Console.OUT.print(j+" ");
		// 	Console.OUT.println("");
		// }
		// 
		// Console.OUT.println("Women rev Preferences");
		// for (i=0n; i<size; i++){
		// 	Console.OUT.print(i+1+": ");
		// 	for(j in revpW(i))
		// 		Console.OUT.print(j+" ");
		// 	Console.OUT.println("");
		// }
	}
	
	// static def readMatrix( fr : FileReader, size : Long,  m1 : Rail[Rail[Int]], 
	// 		  m2 : Rail[Rail[Int]] ){
	// 	try{
	// 		var i : Int = 0n;
	// 		//var charNo : Int = 0n;
	// 		var j : Int;
	// 		var buffer:String;
	// 		var mline:Int=0n;
	// 		var wline:Int=0n;
	// 		
	// 		// Number of the line in which the first pref list entry appears.
	// 		val FIRST_PREF_LINE = 3n;
	// 		
	// 		for (line in fr.lines()) {
	// 			i++;
	// 			buffer = ""; j = 0n;
	// 			if (i >= FIRST_PREF_LINE && i < size + FIRST_PREF_LINE){
	// 				//Console.OUT.println("mp:"+i+" :"+line);
	// 				// Read Men Pref Matrix
	// 				for(char in line.chars() ){
	// 					if( char == ' '){
	// 						if(!buffer.equals("")) {
	// 							if (j < size){
	// 								m1(mline)(j++) = Int.parse(buffer);
	// 								//Console.OUT.println("menPrefs "+(mline)+","+(j-1)+" = "+(mP(mline)(j-1)));
	// 							}
	// 						}
	// 						buffer = "";
	// 					}else{
	// 						buffer += char;
	// 					}                       
	// 				}
	// 				if(!buffer.equals("")) { // process last element
	// 					 if (j < size){
	// 						  m1(mline)(j++) = Int.parse(buffer);
	// 						  //Console.OUT.println("menPrefs "+(mline)+","+(j-1)+" = "+(mP(mline)(j-1)));
	// 					 }
	// 				}
	// 				mline++;
	// 			}else if (i > size + FIRST_PREF_LINE && i <= size * 2 + FIRST_PREF_LINE){
	// 				//Console.OUT.println("wp:"+i+" :"+line);
	// 				// Read Women Pref Matrix
	// 				for(char in line.chars() ){
	// 					if( char == ' ' || char == '\n' ){
	// 						if(!buffer.equals("")) {
	// 							if ( j < size ){
	// 								m2(wline)(j++) = Int.parse(buffer);
	// 								//Console.OUT.println("womenPref "+(wline)+","+(j-1)+" = "+(wP(wline)(j-1)));
	// 							}
	// 						}
	// 						buffer = "";
	// 					}else{
	// 						buffer += char;
	// 					}                       
	// 				}
	// 				if(!buffer.equals("")) {
	// 					 if ( j < size ){
	// 						  m2(wline)(j++) = Int.parse(buffer);
	// 						  //Console.OUT.println("womenPref "+(wline)+","+(j-1)+" = "+(wP(wline)(j-1)));
	// 					 }
	// 				}
	// 				wline++;
	// 			}
	// 		}
	// 	}catch(Exception){
	// 		Console.OUT.println("Error reading file");
	// 		//EOF
	// 	}
	// }
	
	static def readMatrix( fr : FileReader, n1 : Long, n2 : Long, m1 : Rail[Rail[Int]],
			  m2 : Rail[Rail[Int]], getCap : Boolean, cap : Rail[Int] ){
		try{
			var i : Int = 0n;
			//var charNo : Int = 0n;
			var j : Int;
			var countM2 : Int = 0n;
			var buffer:String;
			var m1line : Int = 0n;
			var m2line : Int = 0n;
			val DELIM_CHAR = ' ';
			
			// Number of the line in which the first pref list entry appears.
			val FIRST_PREF_LINE = 3n;
			
			for (line in fr.lines())
			{
				i++;
				buffer = ""; j = 0n; countM2 =0n;
				if (i >= FIRST_PREF_LINE && i < n1 + FIRST_PREF_LINE){ // The file has x header lines (already read)
					// Console.OUT.println("rp:"+i+" :"+line);
					// Read first matrix
					for(char in line.chars() ){
						if( char == DELIM_CHAR ){
							if( !buffer.equals("") )
							{
								if (j < n2)  // maximum number of entries in matrix2
								{
									m1(m1line)(j++) = Int.parse(buffer);
									// Console.OUT.println("resPrefs "+(rline)+","+(j-1)+" = "+(rP(rline)(j-1)));
								}
							}
							buffer = "";
						}else{
							buffer += char;
						}                       
					}
					if( !buffer.equals("") ) // Process last entry in line
					{
						 if (j < n2)        // maximum number of entries in matrix2
 						 { 
							  m1(m1line)(j) = Int.parse(buffer);
							  // Console.OUT.println("resPrefs "+(rline)+","+(j-1)+" = "+(rP(rline)(j-1)));
						 }
					}
					m1line++; 
				}else if (i > n1 + FIRST_PREF_LINE && i <= n1 + n2 + FIRST_PREF_LINE)
				{
					// Console.OUT.println("hp:"+i+" :"+line);
					// Read hospitals Pref Matrix
					for(char in line.chars() )
					{
						if( char == DELIM_CHAR )
						{
							if(!buffer.equals(""))
							{
								if(getCap && j == 0n)  // first entry capacity eg. (cap)
								{
									var capb : String = "";
									for (c in buffer.chars()){
										if (c != '(' && c != ')')
										{
											 capb += c;
										}
									}
									cap(m2line) = Int.parse(capb);
									// Console.OUT.println("capacity of hospital "+ hline+" is "+hcap(hline));
									j++;
								} else if (countM2 < n1 ) 
								{  
									 m2(m2line)(countM2++)= Int.parse(buffer);
									// Console.OUT.println("hosPref "+(hline)+","+(j-1)+" = "+(hP(hline)(j-1)));
									j++;
								}
							}
							buffer = "";
						}else{
							buffer += char; 
						}                       
					}
					if(!buffer.equals("")) // Process last element in list
					{
						 m2(m2line)(countM2) = Int.parse(buffer);
						 // Console.OUT.println("hosPref "+(hline)+","+(j-1)+" = "+(hP(hline)(j-1)));
						 j++;
					}
					m2line++;
				}
			}
		}catch(Exception){
			Console.OUT.println("Error reading file");
			//EOF
		}
	}
	
	
	/** load Parameters Line
	 *  load the parameters of the problems 
	 *  @param filePath path of the data file to be loaded
	 *  @param params rail to return problems parameters on file
	 *  @return true if success, false if filePath is a directory
	 */
	static 
	def tryReadParameters (filePath : String, params : Rail[Long] ):Boolean
	{	 
		 //Load first line with problem's parameters
		 val filep = new File(filePath);
		 
		 if (filep.isDirectory()) return false;
		 
		 Logger.debug(()=>{"\n--   Solving "+filePath+" "});
		 
		 val fr = filep.openRead();
		 var fLine : String;
		 
		 do {
			  fLine = fr.readLine(); //get line
		 }while( fLine( 0n ) == '#'); //ignore first lines that starts with number symbol character
		 
		 val header = readParameters(fLine);
		 
		 //Assign reading parameter to output array
		 for ( var i : Long = 0; i < 4; i++ )
		 {
			  params(i) = header(i);
			  // Console.OUT.println("p "+i+" = "+ params(i));
		 }
		 
		 fr.close();
		 
		 return true;
	}
	
	static def readParameters( line : String ) : Rail[Int]
	{
		var i : Int;
		var j : Int = 0n;
		var buffer : String =  "";
		val x = new Rail[Int]( 4, -1n );
		for(i = 0n ; i < line.length() ; i++) 
		{
			if( line(i) == ' ' || line(i) == '\n' )
			{
				x(j++) = Int.parse(buffer);
				//Console.OUT.println("x "+(j-1)+" = "+x(j-1));
				buffer = "";
			}else
			{
				buffer += line(i);
			}
		}
		x(j) = Int.parse(buffer);
		//Console.OUT.println("x "+j+" = "+x(j));
		return x;
	}
	
	// /**
	//  *  Read Parameters from HRT file
	//  *  Always try to load 4 parameters
	//  * 
	//  */
	// 
	// static def readParametersHR ( line : String ) : Rail[Int] {
	// 	var i : Int;
	// 	var j : Int = 0n;
	// 	var buffer : String =  "";
	// 	val x = new Rail[Int]( 4, -1n );
	// 	
	// 	for( i = 0n ; i < line.length() ; i++ ){
	// 		if( line(i) == ' ' || line(i) == '\n' ){
	// 			x(j++) = Int.parse(buffer);
	// 			//Console.OUT.println("x "+(j-1)+" = "+x(j-1));
	// 			buffer = "";
	// 		}else{
	// 			buffer += line(i);
	// 		}
	// 	}
	// 	x(j) = Int.parse(buffer);
	// 	//Console.OUT.println("x "+j+" = "+x(j));
	// 	return x;
	// }
	
	
	
	/** load dir load the path and create a list of file to be solved
	 *  return by reference the new path in nPath variable
	 *  @param path is the path set by the user
	 *  @param nPath new path tobe used for the program
	 *      (It changes depending if user set a directory or a single file)
	 *  @return the list of files if directory or a list with only one file entry if file 
	 */
	static def loadDir(path : String, nPath:StringBuilder) : Rail[String]{ 
		val fp = new File(path);
		val execList : Rail[String];
		if (fp.isDirectory()){
			Logger.debug(()=>{"solving all problems into this directory"});
			execList = fp.list(); 
			nPath.addString(path);
		}else{
			//Logger.debug(()=>{"Solving "+testNo+" times the problem "+path});
			execList = [fp.getName()];
			nPath.addString(fp.getParentFile().getPath());
			Console.OUT.println(nPath+" "+fp.getName());
		}
		RailUtils.sort(execList);
		return execList;
	}
	
	
	/** load data returning maptable
	 *  load the data in filePath to the data structures mPref and w Pref 
	 *  @param filePath path of the data file to be loaded
	 *  @param matrix1 first matrix (parameter by reference)
	 *  @param matrix2 second matrix (parameter by reference)
	 *  @param mapTable returned map table (parameter by reference)
	 */
	// static def loadData( filePath : String, n1 : Long, n2 : Long, 
	// 		  matrix1 : Rail[Rail[Int]], matrix2 : Rail[Rail[Int]], mapTable : Rail[Int])
	// {
	// 	var loadTime:Long = -System.nanoTime();
	// 	//Load first line wtith headers size p1 p2
	// 	val filep = new File(filePath);//new File(file);//
	// 	
	// 	//if (filep.isDirectory()) return false;
	// 	
	// 	Console.OUT.println("\n--   Solving "+filePath+" ");
	// 	
	// 	val fr = filep.openRead();
	// 	
	// 	//Load Problem
	// 	readMatrix ( fr, n1,  matrix1, matrix2 );
	// 	fr.close();
	// }
	
	/** load data (SMP, QAP)
	 *  load the data in filePath to the data structures mPref and w Pref 
	 *  @param filePath path of the data file to be loaded
	 *  @param matrix1 first matrix (parameter by reference)
	 *  @param matrix2 second matrix (parameter by reference)
	 */
	static def loadData( filePath : String, n1 : Long, n2 : Long, 
			  matrix1 : Rail[Rail[Int]], matrix2 : Rail[Rail[Int]]){
		 var loadTime:Long = -System.nanoTime();
		 //Load first line wtith headers size p1 p2
		 val filep = new File(filePath);//new File(file);//
		 
		 //if (filep.isDirectory()) return false;
		 
		 //Console.OUT.println("\n--   Solving "+filePath+" ");
		 
		 
		 try{
			  val	  fr = filep.openRead();
			  
			  
			  //Load Problem
			  // false in 6th parameter means that it is not necessary to load capacities, 
			  // for that reason the 7th parameter is a dummy rail (won't be used)
			  readMatrix ( fr, n1, n2, matrix1, matrix2, false, new Rail[Int](1,0n) );
			  fr.close();
		 }catch( e : Exception){
			  Console.OUT.println("Error opening file:" + e.getMessage()+" cause "+ e.getCause() );
			  e.printStackTrace();
			  Console.OUT.println("Error " );
			  
		 }
	}
	
	/** load data customized for HRP
	 *  load the data in filePath to the data structures mPref and w Pref 
	 *  @param filePath path of the data file to be loaded
	 *  @param mPref men prefernce list (parameter by reference)
	 *  @param wPref men prefernce list (parameter by reference)
	 *  @return true if success, false if filePath is a directory
	 */
	static def loadData( filePath : String, n1 : Long, n2 : Long, 
			  matrix1 : Rail[Rail[Int]], matrix2 : Rail[Rail[Int]], mapTable : Rail[Int]) 
	{ 
		var loadTime : Long = -System.nanoTime();
		
		//Load first line with headers size p1 p2
		val filep = new File( filePath );         //new File(file);//
		val fr = filep.openRead();
		val hcap = new Rail[Int](n2,0n);  
		//Load Problem
		readMatrix(fr, n1, n2, matrix1, matrix2, true, hcap);
		
		//Create map table for HRP problems
		var pos : Long = n2;
		for(var hi : Int = 0n; hi < n2; hi++){
			 for( var rep : Int = hcap(hi) - 1n; rep > 0n; rep--)
			 {
				  // Console.OUT.println("rep "+rep+" hi "+hi);	  
				  mapTable(pos++) = hi;
				  // Console.OUT.println("maptable "+(pos-1n)+" = "+hi);
			 }
		}
		
		// for(index in mapTable.range())
		// 	 Console.OUT.println("maptable "+index+" = "+mapTable(index));
		
		//Turn HR data into corresponding SMTI (cloning technique) 
		convertRPL( matrix1, hcap, n1, n2);
		convertHPL( matrix2, hcap, n1, n2);
		//printPreferencesTables();
		
		fr.close();
	}	
	
	static def convertRPL(mP:Rail[Rail[Int]],hcap:Rail[Int], n1 : Long, n2 : Long){
		 var ri : Int, hi : Int, rep :Int;
		 var pos : Int = 0n;
		 
		 val accCap = new Rail[Int](n2,0n);
		 var acc:Int=0n;
		 
		 for (var k : Int = 1n; k < n2 ;k++){
			  
			  acc += hcap(k-1)-1n;
			  accCap(k) = acc;
			  // Console.OUT.println("accCap "+k+" ="+accCap(k));
		 }
		 
		 val currentList = new Rail[Int](n1,0n);
		 
		 for( ri = 0n; ri < n1; ri++){
			  pos = 0n;
			  Rail.copy( mP(ri),0, currentList,0,n1 );
			  for (h in currentList.range()){
					val ch = currentList(h);
					if (ch == 0n) break;
					//Console.OUT.println("ch="+ch);
					mP(ri)(pos++) = ch;
					// Console.OUT.println("resPrefs "+ri+","+(pos-1)+" = "+(mP(ri)(pos-1)));
					val capCh = hcap(ch-1); // to index
					//Console.OUT.println("capCh="+capCh);
					for (rep = 1n ; rep < capCh; rep++){ //create ties for the same hospital
						 mP(ri)(pos++) = (n2 as Int + accCap(ch-1) + rep)*-1n;
						 // Console.OUT.println("resPrefs "+ri+","+(pos-1)+" = "+(mP(ri)(pos-1)));
					}
			  }
		 }
	}
	
	static def convertHPL(wP:Rail[Rail[Int]],hcap:Rail[Int], n1 : Long, n2 : Long){
		 var hi : Int, ri : Int, rep :Int;
		 var pos : Int = 0n;
		 
		 val accCap = new Rail[Int](n2,0n);
		 var acc:Int=0n;
		 
		 for (var k:Int = 1n; k < n2 ;k++){
			  
			  acc += hcap(k-1)-1n;
			  accCap(k) = acc;
	//		  Console.OUT.println("accCap "+k+" ="+accCap(k));
		 }
		 
		 val currentList = new Rail[Int](n1,0n);
		 
		 for(hi = 0n; hi<n2 ; hi++){
			  pos = 0n;
			  Rail.copy(wP(hi),0,currentList,0,n1);
			  for (rep = 1n; rep < hcap(hi); rep++){
					val index = n2 + accCap(hi) + rep - 1n;
					// Console.OUT.println("reply list from hospital"+hi +" in line"+index);
					Rail.copy(currentList,0,wP(index),0,n1);
			  }
		 }
	}
}

public type SMTIAS(s:Long)=SMTIAS{self.sz==s};
