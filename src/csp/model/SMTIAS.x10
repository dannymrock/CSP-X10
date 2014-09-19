package csp.model;
import csp.util.Logger;
import csp.util.RandomTools;
import csp.solver.Valuation;
import x10.array.Array_2;
//import x10.io.File;
import x10.io.FileReader;
import x10.io.File;
//import x10.io.FileWriter;
import x10.util.StringBuilder;

import x10.util.RailUtils;

public class SMTIAS extends ModelAS{
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
	val variablesW = new Rail[Int](length,0n);
	/** errV:array with current individual cost (index man - value cost)**/
	val errV = new Rail[Int](length,-1n);
	/** bpi:Blocking par indexes (index man - value bp index to swap)**/
	val bpi = new Rail[Int](length,-1n);
	
	/** weight: weight to compute the total cost (cost = bp*weight + singles)**/
	var weight:Int = length;
	
	var singlei:Int = 0n;
	
	
	var minSingles:Int = 0n;
	
	public def this (lengthProblem : Long , seed : Long, mPrefs:Rail[Rail[Int]], wPrefs:Rail[Rail[Int]], 
			restLimit:Int):SMTIAS(lengthProblem){
		super( lengthProblem, seed );
		this.initParameters(restLimit);
		
		val l = length as Int;
		menPref = mPrefs;
		womenPref = wPrefs;
		revpM = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		revpW = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		// Creating Reverse Matrixes
		var mw:Int,pos:Int;
		var level:Int=0n;
		for(mw = 0n; mw < length; mw++){
			level = 0n;
			var man:Int;
			for(pos=0n; (man = womenPref(mw)(pos)) != 0n; pos++ ){
				if (man > 0n) 
					level++;
				else // if current value is negative = tie, same level as the previous one
					man = -man;
				
				//Convertion to an index	
				revpW(mw)(man - 1) = level;
			}
		}
		
		for(mw = 0n; mw < length; mw++){
			level=0n;
			var woman:Int;
			for(pos=0n; (woman = menPref(mw)(pos)) != 0n; pos++ ){
				if (woman > 0n) 
					level++;
				else // if current value is negative = tie, same level as the previous one
					woman =-woman;
				
				//Converting to an index	
				revpM(mw)(woman - 1) = level;
			}
		}
		/// printPreferencesTables();
	}
	
	/** initParameters
	 *  It is necessary to fine tune the parameters for this problem
	 */
	private def initParameters(rLimit:Int){
		
		solverParams.probSelectLocMin =  100n; // try ~80%  
		solverParams.freezeLocMin = 1n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit =1n; 
		solverParams.resetPercent = 0n;
		solverParams.restartLimit = rLimit;/*30n*length*/; 
		solverParams.restartMax = 0n;
		solverParams.baseValue = 1n;
		solverParams.exhaustive = false;
		solverParams.firstBest = true;
		
		//solverParams.probChangeVector = probCV; //10n;
		
		//weight = System.getenv().get("V")!=null?length:1n;
		//Console.OUT.println("restart Limit= "+solverParams.restartLimit);
		//Console.OUT.println("Prob Change Vector= "+solverParams.probChangeVector);
		
	}
	
	
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
	public def costOfSolution( shouldBeRecorded : Boolean ) : Int {	
		var w:Int;
		var pmi:Int = 0n; // index of current match of mi 
		var pwi:Int = 0n; // index of current match of wi
		var bpnumber:Int = 0n;
		var singles:Int = 0n;
		
		/// if(shouldBeRecorded){
		///  Console.OUT.println("cost of Sol");
		///  Utils.show("conf:",variables);
		/// }
		
		variablesW.clear();
		for (mi in variables.range()){ // mi -> man index (man number = mi + 1)
			variablesW(variables(mi)-1) = mi as Int + 1n;
		}
		
		// verify existence of undomminated BP's for each man 
		for (mi in variables.range()){  // mi -> man index (man number = mi + 1)
			pmi = variables(mi) - 1n; // pm current match of mi (if pm is not valid, mi is single)
			
			var bpMi:Int = -1n;		//NB: -1 to avoid false positive in the test of costIfSwap
			var e:Int = 0n; 	 	
			var levelPM:Int = revpM(mi)(pmi); // m's current match level of preference  
			
			if(levelPM == 0n ){ //verify if m is single (pm is not a valid match)
				levelPM = length + 1n;  //FIX
				singles++;
				if (shouldBeRecorded && r.randomInt(singles) == 0n){
					singlei = mi as Int;
				}
			}
			
			var levelW:Int = 0n;
			var li:Long=0;
			for(li=0;(w=menPref(mi)(li))!=0n;li++){
				if(w > 0n)
					levelW++;			// new level of preference
				else					 
					w = -w;             // if w < 0 -> same level of preference (tie), "restore" w
				
				if (levelW >= levelPM) // stop if cuerrent level of pref is bigger or equal 
					break;             // than the level of pref of pm (current match) "stop condition"  
				
				pwi = variablesW(w-1)-1n; // pwi index of the current match of the woman w
				
				e = blockingPairError(w-1n, pwi, mi as int);  // check if w prefers m to pw 
				if (e > 0n){	
					bpMi = pwi; 
					bpnumber++;     // count the errors (number of BP)
					break; 			// only consider undominated BP
				}
			}
			if (shouldBeRecorded){
				errV(mi) = e;
				bpi(mi) = bpMi;
				///Console.OUT.println("mi= "+mi+" e= "+e+" bpMi= "+bpMi);
			}
			
		}
		if (shouldBeRecorded) {
			nbBP = bpnumber;
			nbSingles = singles;
			///Console.OUT.println("totalCost= "+(bpnumber*weight+singles));
		}
		
		//         val s2 : Int = 2n * singles - length;  // check the case where singles is an odd number, is it correct to use round up, ie. (singles +1) / 2 ?
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
	public def costOnVariable( i : Int ) : Int {		
		return errV(i);
	}
	
	
	public def nextJ(i:Int, j:Int, exhaustive:Int) : Int {
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
	public def executedSwap ( i1 : Int, i2 : Int) {
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
	public def costIfSwap(current_cost:Int,var i1:Int, var i2:Int) : Int {
		swapVariables(i1, i2);
		var r : Int = costOfSolution(false);
		swapVariables(i1, i2);
		return r;
	}
	
	
	public def findMax(pvalue1:Int, pvalue2:Int):Int { //pvalue is an man index
		var maxi:Int = -1n;		/* return -1 if none found */
		var maxErr:Int = 0n;
		var maxNb:Int = 0n;
		var i:Int;
		
		for(i = 0n; i < length; i++){
			var e:Int = errV(i);
			if (e <= 0n || i == pvalue1 || i == pvalue2 ||  bpi(i) == pvalue1 || bpi(i) == pvalue2) 
				continue;
			if (e > maxErr){ 	
				maxi = i;
				maxErr = e;
				maxNb = 1n;
			} 
			else if (e == maxErr && r.randomInt(++maxNb) == 0n){
				maxi = i;
			}
		}
		// val mi = maxi;
		// Logger.debug(()=>"maxi:"+mi);
		return maxi;
	}
	
	public def reset ( var n : Int, totalCost : Int ) : Int {			
		
		// 1st BLOCKING PAIRS
		if (nbBP > 0n){
			var maxi:Int = findMax(-1n, -1n);	/* find max */
			var bpiMaxi:Int = bpi(maxi);
			var otheri:Int; 
			///Console.OUT.println("Reset maxi= "+maxi+" bpiMaxi = "+ bpiMaxi);
			swapVariables(maxi, bpiMaxi);
			if (nbBP > 1n && r.randomDouble() < 0.98 &&  (otheri = findMax(maxi,bpiMaxi)) >= 0n){
				///Console.OUT.println("Reset otheri= "+otheri+" bpi(otheri) = "+ bpi(otheri));
				swapVariables(otheri, bpi(otheri));
				return -1n;
			}
		}
		
		if (nbSingles > 0) {
			val j = r.randomInt(length);
			///Console.OUT.println("Reset single singV("+singlei+")= "+singV(singlei)+" random j = "+ j+"  nbSingles="+nbSingles);
			swapVariables(singlei, j);		
		} else {
			val i = r.randomInt(length);
			val j = r.randomInt(length);
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
	
	public def verify(match:Valuation(sz)):Boolean {
		var w:Int;
		var pmi:Int = 0n;
		var pwi:Int = 0n; //w_of_m, m_of_w;
		var r:Int = 0n;
		var singles:Int = 0n;
		
		val permutV = new Rail[Int](sz, 0n);
		val variablesWv = new Rail[Int](length,0n);
		for (mi in match.range()){
			val value = match(mi);
			permutV(value-1)++;
			if (permutV(value-1)>1){
				Console.OUT.println("Not valid permutation, value "+ value +" is repeted");
			}
			if (value==0n)	Console.OUT.println("not valid Zero in solution");
			variablesWv(value-1) = mi as Int + 1n;
		}
		
		// verify existence of undomminated BP's for each man 
		for (mi in match.range()){  // mi -> man index (man number = mi + 1)
			pmi = match(mi)-1n; // pm current match of mi 
			var e:Int = 0n; 	 	
			var bF:Int = 0n;
			var levelPM:Int = -1n; //m's current match level of preference  
			
			if( revpM(mi)(pmi)==0n ){
				levelPM = length; //put some value
				singles++;
				Console.OUT.println("Error m="+ (mi+1n) +" w="+(pmi+1n)+" is not a valid match (single)");
			} else{ // m has a valid assignment pm
				levelPM = revpM(mi)(pmi);
			}
			
			var levelW:Int = 0n;
			for(li in menPref(mi).range()){ //li level of preference index
				
				w = menPref(mi)(li);
				if (w == 0n) continue;	// entry deleted
				if(w > 0n)			// new level of preference
					levelW++;
				else						// if w < 0 -> same level of preference (tie) 
					w = -w;
				if (levelW >= levelPM) break; //stop if cuerrent level of pref is bigger or equal 
				// than the level of pref of pm (current match) "stop condition"
				pwi = variablesWv(w-1)-1n; //pw current match of the current
				// 	// Verify if w prefers m to pw
				e = verifyBlockingPairError(w-1n, pwi, mi as Int);
				
				if (e > 0n){
					r++;
					Console.OUT.println("Error: blocking pair m="+(mi+1n)+" w="+w+" pw= "+(pwi+1n) +" with error= "+e);
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
	
	/**
	 * 	Set the parameter in the solver
	 * 	@param solverParameters Solver parameter from the model
	 */
	public def setParameters(solverParameters : ASSolverParameters):void{
		solverParameters.setValues(solverParams);
	}
	
	public def initialize( baseValue : Int ) {
		for(k in variables.range()){
			variables(k) = baseValue + k as Int;
		}
		//Main.show("before ini",variables);
		for( var i:Int = length - 1n ; i >	0n ; i-- ) {
			val j = r.randomInt( i + 1n );
			swapVariables(i,j);
		}
	}
	
	public def swapVariables(i:Int, j:Int):void{
		//Console.OUT.println("swap func i: "+i+" j: "+j);
		val x = variables(i);
		variables(i) = variables(j); 
		variables(j) = x;
	}
	
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
		Console.OUT.println("\nMatching  m->w:");
		for (i in match.range()){
			if(revpM(i)(match(i)-1n)==0n){
				Console.OUT.printf("%4d->%-4d",(i+1),0n);
			}else
				Console.OUT.printf("%4d->%-4d",(i+1),variables(i));
		}
		Console.OUT.print("\n");
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
	
	private def printPreferencesTables(){
		Console.OUT.println("\nMen Preferences");
		var i:Int = 0n;
		for (i=0n; i<length; i++){
			Console.OUT.print(i+1+": ");
			for(j in menPref(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
		Console.OUT.println("Women Preferences");
		for (i=0n; i<length; i++){
			Console.OUT.print(i+1+": ");
			for(j in womenPref(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
		
		Console.OUT.println("Men rev Preferences");
		for (i=0n; i<length; i++){
			Console.OUT.print(i+1+": ");
			for(j in revpM(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
		
		Console.OUT.println("Women rev Preferences");
		for (i=0n; i<length; i++){
			Console.OUT.print(i+1+": ");
			for(j in revpW(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
	}
	
	static def readMatrix(fr:FileReader, sizeF:Int,  mP:Rail[Rail[Int]], wP:Rail[Rail[Int]]){
		try{
			var i : Int = 0n;
			//var charNo : Int = 0n;
			var j : Int;
			var buffer:String;
			var mline:Int=0n;
			var wline:Int=0n;
			
			for (line in fr.lines()) {
				i++;
				buffer = ""; j = 0n;
				if (i >= 2n && i < sizeF + 2){
					//Console.OUT.println("mp:"+i+" :"+line);
					// Read Men Pref Matrix
					for(char in line.chars() ){
						if( char == ' ' || char == '\n' ){
							if(!buffer.equals("")) {
								if (j < sizeF){
									mP(mline)(j++) = Int.parse(buffer);
									//Console.OUT.println("menPrefs "+(mline)+","+(j-1)+" = "+(mP(mline)(j-1)));
								}
							}
							buffer = "";
						}else{
							buffer += char;
						}                       
					}
					mline++;
				}else if (i > sizeF + 2 && i <= sizeF * 2 + 2){
					//Console.OUT.println("wp:"+i+" :"+line);
					// Read Women Pref Matrix
					for(char in line.chars() ){
						if( char == ' ' || char == '\n' ){
							if(!buffer.equals("")) {
								if (j < sizeF){
									wP(wline)(j++)= Int.parse(buffer);
									//Console.OUT.println("womenPref "+(wline)+","+(j-1)+" = "+(wP(wline)(j-1)));
								}
							}
							buffer = "";
						}else{
							buffer += char;
						}                       
					}
					wline++;
				}
			}
		}catch(Exception){
			Console.OUT.println("Error reading file");
			//EOF
		}
	}
	static def readMatrixHR(fr:FileReader, n1:Int, n2:Int, rP:Rail[Rail[Int]], hP:Rail[Rail[Int]],hcap:Rail[Int]){
		try{
			var i : Int = 0n;
			//var charNo : Int = 0n;
			var j : Int;
			var buffer:String;
			var rline:Int=0n;
			var hline:Int=0n;
			
			for (line in fr.lines()) {
				i++;
				buffer = ""; j = 0n;
				if (i >= 2n && i < n1 + 3){ // The file has 3 header lines (already read)
					// Console.OUT.println("rp:"+i+" :"+line);
					// Read residents Pref Matrix
					for(char in line.chars() ){
						if( char == ' ' || char == '\n' ){
							if(!buffer.equals("")) {
								if (j < n2){       // maximum number of entries in hospital pref list
									rP(rline)(j++) = Int.parse(buffer);
									// Console.OUT.println("resPrefs "+(rline)+","+(j-1)+" = "+(rP(rline)(j-1)));
								}
							}
							buffer = "";
						}else{
							buffer += char;
						}                       
					}
					rline++; 
				}else if (i > n1 + 2 && i <= n1 + n2 + 2){
					// Console.OUT.println("hp:"+i+" :"+line);
					// Read hospitals Pref Matrix
					for(char in line.chars() ){
						if( char == ' ' || char == '\n' ){
							if(!buffer.equals("")) {
								if(j == 0n){ // first entry in line is the hospital capacity
									 var cap:String="";
									for(c in buffer.chars()){
										if (c != '(' && c != ')'){
											 cap += c;
										}
									}
									 hcap(hline) = Int.parse(cap);
									 // Console.OUT.println("capacity of hospital "+ hline+" is "+hcap(hline));
									 j++;
								}
								else if (j < n1 + 1){   // maximum number of entries in hospital pref list (n2 + 1 for capacity)
									hP(hline)(j-1)= Int.parse(buffer);
									// Console.OUT.println("hosPref "+(hline)+","+(j-1)+" = "+(hP(hline)(j-1)));
									j++;
								}
							}
							buffer = "";
						}else{
							buffer += char; 
						}                       
					}
					hline++;
				}
			}
		}catch(Exception){
			Console.OUT.println("Error reading file");
			//EOF
		}
	}
	
	static def readParameters(line : String):Rail[Int]{
		var i : Int;
		var j : Int = 0n;
		var buffer:String =  "";
		val x = new Rail[Int](3,0n);
		for(i = 0n ; i < line.length() ; i++){
			if( line(i) == ' ' || line(i) == '\n' ){
				x(j++) = Int.parse(buffer);
				//Console.OUT.println("x "+(j-1)+" = "+x(j-1));
				buffer = "";
			}else{
				buffer += line(i);
			}
		}
		x(j) = Int.parse(buffer);
		//Console.OUT.println("x "+j+" = "+x(j));
		return x;
	}
	
	static def readParametersHR(line : String):Rail[Int]{
		var i : Int;
		var j : Int = 0n;
		var buffer:String =  "";
		val x = new Rail[Int](3,0n);
		for(i = 0n ; i < line.length() ; i++){
			if( line(i) == ' ' || line(i) == '\n' ){
				x(j++) = Int.parse(buffer);
				//Console.OUT.println("x "+(j-1)+" = "+x(j-1));
				buffer = "";
			}else{
				buffer += line(i);
			}
		}
		x(j) = Int.parse(buffer);
		//Console.OUT.println("x "+j+" = "+x(j));
		return x;
	}
	
	
	
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
	
	
	/** load data
	 *  load the data in filePath to the data structures mPref and w Pref 
	 *  @param filePath path of the data file to be loaded
	 *  @param mPref men prefernce list (parameter by reference)
	 *  @param wPref men prefernce list (parameter by reference)
	 *  @return true if success, false if filePath is a directory
	 */
	static def loadData(filePath : String, mPref:Rail[Rail[Int]],wPref:Rail[Rail[Int]]):Boolean{
		var loadTime:Long = -System.nanoTime();
		//Load first line wtith headers size p1 p2
		val filep = new File(filePath);//new File(file);//
		if (filep.isDirectory()) return false;
		
		Console.OUT.println("\n--   Solving "+filePath+" ");
		
		val fr = filep.openRead();
		val fLine = fr.readLine(); //get first line
		val header = readParameters(fLine);
		
		val sizeF = header(0); val p1F = header(1); val p2F = header(2);
		Logger.debug(()=>{"file: "+filePath+" size: "+sizeF+" p1: "+p1F+" p2: "+p2F});
		
		//Load Problem
		readMatrix(fr, sizeF,  mPref, wPref);
		fr.close();
		return true;
	}
	
	/** load data customized for HRP
	 *  load the data in filePath to the data structures mPref and w Pref 
	 *  @param filePath path of the data file to be loaded
	 *  @param mPref men prefernce list (parameter by reference)
	 *  @param wPref men prefernce list (parameter by reference)
	 *  @return true if success, false if filePath is a directory
	 */
	static def loadDataHR(filePath : String, mPref:Rail[Rail[Int]],wPref:Rail[Rail[Int]]):Boolean{
		var loadTime:Long = -System.nanoTime();
		//Load first line wtith headers size p1 p2
		val filep = new File(filePath);//new File(file);//
		if (filep.isDirectory()) return false;
		
		Console.OUT.println("\n--   Solving "+filePath+" ");
		
		val fr = filep.openRead();
		val line1 = fr.readLine(); //ignore first line commented line with all parameters  
		val line2 = fr.readLine(); //get second line - read the parameters needed for the program
		val header = readParametersHR(line2);
		
		val n1 = header(0); val n2 = header(1); val c = header(2);
		Logger.info(()=>{"file: "+filePath+" n1: "+n1+" n2: "+n2+" c: "+c});
		
		val hcap = new Rail[Int](n2,0n);  
		//Load Problem
		readMatrixHR(fr, n1, n2, mPref, wPref, hcap);
		
		//Turn HR data into corresponding SMTI
		convertRPL(mPref,hcap, n1, n2);
		convertHPL(wPref,hcap, n1, n2);
		//printPreferencesTables();
		
		fr.close();
		return true;
	}
	
	static def convertRPL(mP:Rail[Rail[Int]],hcap:Rail[Int], n1 : Int, n2 : Int){
		 var ri : Int, hi : Int, rep :Int;
		 var pos : Int = 0n;
		 
		 val accCap = new Rail[Int](n2,0n);
		 var acc:Int=0n;
		 
		 for (var k:Int = 1n; k < n2 ;k++){
			  
			  acc += hcap(k-1)-1n;
			  accCap(k) = acc;
			  // Console.OUT.println("accCap "+k+" ="+accCap(k));
		 }
		 
		 for(ri=0n; ri<n1 ; ri++){
			  val currentList = new Rail[Int](n1,0n);
			  pos = 0n;
			  Rail.copy(mP(ri),currentList);
			  for (h in currentList.range()){
					val ch = currentList(h);
					if (ch == 0n) break;
					//Console.OUT.println("ch="+ch);
					mP(ri)(pos++) = ch;
					// Console.OUT.println("resPrefs "+ri+","+(pos-1)+" = "+(mP(ri)(pos-1)));
					val capCh = hcap(ch-1); // to index
					//Console.OUT.println("capCh="+capCh);
					for (rep = 1n ; rep < capCh; rep++){ //create ties for the same hospital
						 mP(ri)(pos++) = (n2 + accCap(ch-1) + rep)*-1n;
						 // Console.OUT.println("resPrefs "+ri+","+(pos-1)+" = "+(mP(ri)(pos-1)));
					}
			  }
		 }
	}
	
	static def convertHPL(wP:Rail[Rail[Int]],hcap:Rail[Int], n1 : Int, n2 : Int){
		 var hi : Int, ri : Int, rep :Int;
		 var pos : Int = 0n;
		 
		 val accCap = new Rail[Int](n2,0n);
		 var acc:Int=0n;
		 
		 for (var k:Int = 1n; k < n2 ;k++){
			  
			  acc += hcap(k-1)-1n;
			  accCap(k) = acc;
	//		  Console.OUT.println("accCap "+k+" ="+accCap(k));
		 }
		 
		 for(hi = 0n; hi<n2 ; hi++){
			  val currentList = new Rail[Int](n1,0n);
			  pos = 0n;
			  Rail.copy(wP(hi),currentList);
			  for (rep = 1n; rep < hcap(hi); rep++){
					val index = n2 + accCap(hi) + rep - 1n;
					// Console.OUT.println("reply list from hospital"+hi +" in line"+index);
					Rail.copy(currentList,wP(index));
			  }
		 }
	}
}

public type SMTIAS(s:Long)=SMTIAS{self.sz==s};
