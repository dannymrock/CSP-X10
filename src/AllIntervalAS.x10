public class AllIntervalAS extends ModelAS {
	 
	val nbOcc : Array[Int];		/* nb occurrences (to compute total cost) 0 is unused */
	
	def this ( val lengthProblem : Int , seed : Long ){
		super( lengthProblem, seed );
		nbOcc = new Array[Int]( varRegion , 0 );
	}
	
	
	private def initParameters(){
		
		solverParams.probSelectLocMin = 66;
		solverParams.freezeLocMin = 1;
		solverParams.freezeSwap = 0;
		solverParams.resetLimit = 1;
		solverParams.resetPercent = 25;
		solverParams.restartLimit = 10000000;
		solverParams.restartMax = 0;
		solverParams.baseValue = 0;
		solverParams.exhaustive = false;
		solverParams.firstBest = false;
	} 
	
	
	public def cost(nbOcc : Array[Int]) : Int //int nb_occ[]
	{
		var r : Int = 0;
		var i : Int;

		for(i = 1; i < length; i++)
			if (nbOcc(i) == 0)
				r += i;
		
		return r;
	}
	
	
	public def costOfSolution ( ) : Int
	{
		var i : Int;

		nbOcc.clear();
		
		for(i = 0; i < length - 1; i++){
			val aux = Math.abs(variables(i) - variables(i + 1)); 
			nbOcc(aux) = nbOcc(aux) + 1;
		}
		
		if (Is_Trivial_Solution(variables, length))
			return length;
		
		return cost(nbOcc);
	}
	
	public def Is_Trivial_Solution(sol : Array[Int], size : Int) : Boolean
	{
		return ( 	sol(0) == 0 || sol(0) == size - 1 || sol(size - 1) == 0 || sol(size - 1) == size - 1);
	}
	
	public def costIfSwap(current_cost:Int, i1:Int, i2:Int):Int
	{
		var s1 : Int;
		var s2 : Int;
		var rem1 : Int;
		var rem2 : Int;
		var rem3 : Int;
		var rem4 : Int;
		var add1 : Int;
		var add2 : Int;
		var add3 : Int;
		var add4 : Int;

		if ((i1 == 0 && (variables(i2) == 0 || variables(i2) == length - 1)) ||
				(i2 == 0 && (variables(i1) == 0 || variables(i1) == length - 1)))
			return length;

		s1 = variables(i1);
		s2 = variables(i2);

		if (i1 > 0)
		{
			rem1 = Math.abs(variables(i1 - 1) - s1); 
			nbOcc(rem1) = nbOcc(rem1) - 1; 
			add1 = Math.abs(variables(i1 - 1) - s2); 
			nbOcc(add1) = nbOcc(add1) + 1; 
		}
		else
			rem1 = add1 = 0;


		if (i1 < i2 - 1)		// i1 and i2 are not consecutive 
		{
			rem2 = Math.abs(s1 - variables(i1 + 1));
			nbOcc(rem2) = nbOcc(rem2) - 1; 
			add2 = Math.abs(s2 - variables(i1 + 1));
			nbOcc(add2) = nbOcc(add2) + 1; 

			rem3 = Math.abs(variables(i2 - 1) - s2); 
			nbOcc(rem3) = nbOcc(rem3) - 1; 
			add3 = Math.abs(variables(i2 - 1) - s1);
			nbOcc(add3) = nbOcc(add3) + 1; 
		}
		else
			rem2 = add2 = rem3 = add3 = 0;

		if (i2 < length - 1)
		{
			rem4 = Math.abs(s2 - variables(i2 + 1)); 
			nbOcc(rem4) = nbOcc(rem4) - 1;
			add4 = Math.abs(s1 - variables(i2 + 1));
			nbOcc(add4) = nbOcc(add4) + 1;
		}
		else
			rem4 = add4 = 0;

		var r : Int = cost(nbOcc);

		// undo 

		nbOcc(rem1) = nbOcc(rem1) + 1;
		nbOcc(rem2) = nbOcc(rem2) + 1;
		nbOcc(rem3) = nbOcc(rem3) + 1;
		nbOcc(rem4) = nbOcc(rem4) + 1; 
		nbOcc(add1) = nbOcc(add1) - 1;
		nbOcc(add2) = nbOcc(add2) - 1;
		nbOcc(add3) = nbOcc(add3) - 1;
		nbOcc(add4) = nbOcc(add4) - 1;

		return r;
	}
	
	
	public def executedSwap( i1 : Int, i2 : Int) {
		var s1 : Int;
		var s2 : Int;
		var rem1 : Int;
		var rem2 : Int;
		var rem3 : Int;
		var rem4 : Int;
		var add1 : Int;
		var add2 : Int;
		var add3 : Int;
		var add4 : Int;

		s1 = variables(i2);			// swap already executed 
		s2 = variables(i1);

		if (i1 > 0)
		{
			rem1 = Math.abs(variables(i1 - 1) - s1); 
			nbOcc(rem1) = nbOcc(rem1) - 1; 
			add1 = Math.abs(variables(i1 - 1) - s2); 
			nbOcc(add1) = nbOcc(add1) + 1; 
		}

		if (i1 < i2 - 1)              // i1 and i2 are not consecutive 
		{
			rem2 = Math.abs(s1 - variables(i1 + 1)); 
			nbOcc(rem2) = nbOcc(rem2) - 1; 
			add2 = Math.abs(s2 - variables(i1 + 1)); 
			nbOcc(add2) = nbOcc(add2) + 1; 

			rem3 = Math.abs(variables(i2 - 1) - s2);
			nbOcc(rem3) = nbOcc(rem3) - 1; 
			add3 = Math.abs(variables(i2 - 1) - s1); 
			nbOcc(add3) = nbOcc(add3) + 1; 
		}

		if (i2 < length - 1)
		{
			rem4 = Math.abs(s2 - variables(i2 + 1)); 
			nbOcc(rem4) = nbOcc(rem4) - 1;
			add4 = Math.abs(s1 - variables(i2 + 1)); 
			nbOcc(add4) = nbOcc(add4) + 1;
		}
	}
	
	
	public def reset( n : Int): Int // AdData *p_ad
	{
		var distMin : Int = length - 3;	// size - 1 also works pretty well 
		var i : Int;
		var j : Int;
		
		for(i = 1; i < length; i++)
		{
			if (Math.abs(variables(i - 1) - variables(i)) >= distMin)
			{
				j = r.randomInt(length);
				this.swapVariables(i,j);
				//j = Random(length);
				//Ad_Swap(i, j);
			}
		}
		return -1;
	}
	
}