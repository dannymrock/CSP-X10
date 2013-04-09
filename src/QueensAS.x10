//package fr.univ.paris1.cri.CSPSolver.Model;
import x10.util.Random; 

public class QueensAS extends ModelAS {
	val nb_diag : Int; 
	val err_d1 : Array[Int](1);
	val err_d2 : Array[Int](1);
	val updateTbl: Array[UpdateErr](1);
	
	class UpdateErr{ 
		var diagIndex:Int;
		var toAdd:Int;
		var diagType:Int;
		public def this(){
			this.toAdd = 0;
			this.diagIndex = 0;
			this.diagType = 0;
		}
	}
	
	public def this(val lengthProblem: Int,seed:Long) {
		super(lengthProblem,seed);
		nb_diag = 2 * length - 1;
		err_d1 = new Array[Int](1..nb_diag , 0);
		err_d2 = new Array[Int](1..nb_diag , 0);
		updateTbl = new Array[UpdateErr](1..8);
		// to set solver parameters
		solverParams.firstBest = true;
		solverParams.probSelectLocMin = 6;
		solverParams.freezeLocMin = 3;
		solverParams.freezeSwap = 0;
		solverParams.resetLimit = length / 5;
		solverParams.resetPercent = 10;
		solverParams.restartLimit = 10000000;
		solverParams.restartMax = 0;
		solverParams.exhaustive = false;
	}
	
	private def ini(){
		for(i in updateTbl)
			updateTbl(i) = new UpdateErr();
	}
	
	public def costOfSolution():Int{
		var d : Int;
		var i : Int;
		var j : Int;
		var aux : Int;
		var er : Int;
		var r : Int = 0;
		err_d1.clear();
		err_d2.clear();
		
		for(i = 1; i < length+1; i++)
		{
			j = variables(i);  					//#define D1(i, j)      (i + size1 - j)
			//aux =;				// err_d1 first position index 1
			err_d1( i + length - j)++;// = err_d1(aux)+1;		//#define D2(i, j)      (i + j)
			//aux = ;					// err_d1 first position index 1
			err_d2(i + j - 1)++;// = err_d2(aux)+1;		//#define ErrD1(i, j)   (err_d1[D1(i, j)])
		}										//#define ErrD2(i, j)   (err_d2[D2(i, j)])
	
		//r = 0;
		for(d = 2; d < nb_diag ; d++)
		{
			er = err_d1(d);
			r += ((er <= 1) ? 0 : er);
	
			er = err_d2(d);
			r += ((er <= 1) ? 0 : er);
		}
		return r;
	}
	
	
	/*
	 *  COST_ON_VARIABLE
	 * 
	 *  Evaluates the error on a variable.
	 */
	public def costOnVariable(var i:Int):Int{
		var j:Int;
		var r:Int;
		var x:Int;
		var aux : Int;
		
		j = variables(i);
		
		x = err_d1(i + length - j);
		r = ((x <= 1) ? 0 : x);
		x = err_d2(i + j - 1);
		r += ((x <= 1) ? 0 : x);
		
		return r;
	}
	
	
	public def costIfSwap(var current_cost:Int, var i1:Int, var i2:Int):Int
	{
		var r:Int = 0;
		var x:Int;
		var err:Int;
		
		var j1:Int;
		var j2:Int;
		
		
		var aux:Int=0;
		var add:Int=0;
		
		var start:Int=1; //Firsst position in the Array
		var end:Int=1;
		
		j1 = variables(i1);
 		j2 = variables(i2);
 		
 		this.ini();
 		
 		end = updateErrorTable(updateTbl, start, end, 0,(i1 + length - j1), -1 );
 		end = updateErrorTable(updateTbl, start, end, 0,(i2 + length - j2), -1 );
 		end = updateErrorTable(updateTbl, start, end, 0,(i1 + length - j2), 1 );
 		end = updateErrorTable(updateTbl, start, end, 0,(i2 + length - j1), 1 );
 		
 		start=end;
 		end = updateErrorTable(updateTbl, start, end, 1,(i1 + j1 - 1), -1 );
 		end = updateErrorTable(updateTbl, start, end, 1,(i2 + j2 - 1), -1 );
 		end = updateErrorTable(updateTbl, start, end, 1,(i1 + j2 - 1), 1 );
 		end = updateErrorTable(updateTbl, start, end, 1,(i2 + j1 - 1), 1 );

 		r = current_cost;
 		for(var k:Int=1; k<end;k++){
 			if(updateTbl(k).diagType == 0){
	 			x = err_d1(updateTbl(k).diagIndex);
	 			r -= ((x <= 1) ? 0 : x);
	 			x += updateTbl(k).toAdd;
	 			r += ((x <= 1) ? 0 : x);
 			}else{  //May be i can eliminate this branch whit 1 err_D vector
 				x = err_d2(updateTbl(k).diagIndex);
 				r -= ((x <= 1) ? 0 : x);
 				x += updateTbl(k).toAdd;
 				r += ((x <= 1) ? 0 : x);
 			}
 		}
  		return r;
	}
	
	private def updateErrorTable (val updateTbl:Array[UpdateErr](1), val start:Int, val end:Int, val diagtype:Int,
			val diagNo:Int, val toAdd:Int ): Int{
		
		for (var i:Int=start; i<end+1;i++){
			if(updateTbl(i).diagType == diagtype)
				if (updateTbl(i).diagIndex==diagNo){
					updateTbl(i).toAdd += toAdd;
					return end;
				}
		}
		updateTbl(end).diagType=diagtype;
		updateTbl(end).diagIndex=diagNo;
		updateTbl(end).toAdd = toAdd;
		
		return end + 1 ;
	}
			
	public def executedSwap(var i1:Int, var i2:Int){
		var j1:Int;
		var j2:Int;
		var aux:Int;
		
		j1 = this.variables(i2);		/* swap already executed */
		j2 = this.variables(i1);

		
		aux = i1 + length - j1;
		err_d1(aux)--;
		aux = i1 + j1 - 1;
		err_d2(aux)--;	
		aux = i2 + length - j2;
		err_d1(aux)--;	
		aux = i2 + j2 - 1;
		err_d2(aux)--;	
 
		aux = i1 + length - j2;
		err_d1(aux)++;	
		aux = i1 + j2 - 1;
		err_d2(aux)++;	
		aux = i2 + length - j1;
		err_d1(aux)++;	
		aux = i2 + j1 - 1;
		err_d2(aux)++;	
	}
}

