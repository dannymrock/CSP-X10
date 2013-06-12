import x10.util.Random;
public class ASSolverPermutTLP extends ASSolverPermut{
	
	val THNUM : Int;
	val partition : Int; 
	// val listIndexJNb : Array[Int];
	// val listNewCost : Array[Int];
	// val minth : Array[Int];
	
	//val listIth1 : Array[Int]; 
	//val listIth2 : Array[Int];  
	//val listIndexNb : Array[Int];
	//val listCost : Array[Int];
	//val varMarked : Array[Int];
	val distData : Rail[ThreadData];

	
	public def this( sizeOfProblem : Int , seed : Long, conf : ASSolverConf, nbAct : Int) {
		super(sizeOfProblem, seed, conf);
		THNUM = nbAct;
		partition = size / THNUM;
		//Console.OUT.println("THNUM "+THNUM+" partition"+partition);
		// listIndexJNb =  new Array[Int](1..THNUM, 0);
		// listNewCost = new Array[Int](1..THNUM, total_cost);
		// minth = new Array[Int](1..THNUM, 0);
		distData = new Rail[ThreadData](0..(THNUM-1));
		
		//listIth1 = new Array[Int](0..partition,0);
		//listIth2 = new Array[Int](0..partition,0);
		//listIndexNb =  new Array[Int](1..THNUM,0);
		//listCost = new Array[Int](1..THNUM,0);
		//varMarked = new Array[Int](1..THNUM,0);
		
		for (i in distData)
			distData(i) = new ThreadData(partition); 
	}

	/**
	 * 	selectVarHighCost( csp : ModelAS ) : Int
	 * 	Select the maximum cost variable of the problem 
	 *  Also computes the number of marked variables.
	 *  @param csp problem model
	 * 	@return the index of the variable with high individual cost
	 */
	public def selectVarHighCost( csp : ModelAS ) : Int{
		//Console.OUT.println("In");
		//var i: Int;
		//var x: Int;
		//var max: Int; 
		
		list_i_nb = 0; //Number of elements
		//max = 0;
		nb_var_marked = 0;
		
		//listIth1.clear();
		//listIth2.clear();
		//listIndexNb.clear();
		//listCost.clear();
		//varMarked.clear();
				
		//i = -1; 
		//Console.OUT.println("Aqui");
		//Console.OUT.println("parallelRules");	
		finish for(th in distData){
			async{
				var i : Int;
				var x : Int;
				//var max : Int;
				//var maxIth:Int = -1;
				var localMarked : Int = 0; 
				//val r = new Random();
				
				distData(th).nbIndex = 0;
				distData(th).cost = 0;
				distData(th).varMarked = 0;
				//val regth = (((th-1)*partition)+1)..(th*partition);
				for (i = (th(0)*partition); i < (th(0)+1)*partition; i++)		//while(i++ < size) 
				{	
					if (nbSwap < mark(i))
					{
						distData(th).varMarked = distData(th).varMarked + 1; 
						continue;
					}
					//Console.OUT.println("Aqui");
					x = csp.costOnVariable(i);
					//Console.OUT.println("var: "+i+" cost= "+x);
					if (x >= distData(th).cost){
						if (x > distData(th).cost){
							distData(th).cost = x;
							distData(th).nbIndex = 0;
						}
						distData(th).listIth(distData(th).nbIndex) = i;
						//if (th==1) listIth1(listIndexNb(th)) = i;
						//else listIth2(listIndexNb(th)) = i;
						distData(th).nbIndex = distData(th).nbIndex + 1;
					}
				}
			}
		}
		
		//= varMarked(1)+varMarked(2);
		
		
		var nbeqth : Int = 0;
		val eqIndex = new Rail[Int](0..(THNUM-1),0);
		var maxCost : Int = -1;
		
		
		for(id in distData){
			nb_var_marked +=  distData(id).varMarked;
			if(distData(id).cost > maxCost){
				maxCost = distData(id).cost;
				nbeqth = 0;
				eqIndex(nbeqth) = id(0);
				nbeqth++;
			}else if(distData(id).cost == maxCost){
				eqIndex(nbeqth) = id(0);
				nbeqth++;
			}
		}
		
		//Make vector List_I
		var limit:Int;
		var antLimit:Int = 0;
		var thindex:Int;
		for(var h : Int = 0; h < nbeqth; h++){
			thindex = eqIndex(h);
			limit = distData(thindex).nbIndex;
			Array.copy(distData(thindex).listIth , 0 , list_i , antLimit , limit);
			antLimit+=limit;
		}
		list_i_nb = antLimit;
			
		nbSameVar += list_i_nb;
		val ranIndex = random.randomInt(list_i_nb);
		return list_i(ranIndex);
	}
	
	/**
	 * 	selectVarMinConflict( csp : ModelAS) : Int
	 * 	Computes swap and selects the minimum of cost if swap
	 * 	@param csp problem model
	 * 	@return the index of the variable with minimum individual cost if swap
	 */
	/*public def selectVarMinConflict( csp : ModelAS) : Int {
		
		var flagOut:Boolean = false; 
		var lmin_j : Int = -1;
				
		do{
			flagOut = false;
			list_j_nb = 0;
			new_cost = total_cost;
			
			listIndexJNb.clear();
			listNewCost(1)=total_cost;
			listNewCost(2)=total_cost;
			minth.clear();
			
			finish for(th in 1..THNUM){
				async{
					var x: Int;
					val randomth = new Random();
					//while(++j < size) 
					var j :Int = 0;
					for (j = ((th-1)*partition); j < th*partition; j++)		//while(i++ < size) 
					{
						//Console.OUT.println("swap "+j+"/"+max_i);
						x = csp.costIfSwap(total_cost, j, max_i);
						//Console.OUT.println("swap "+j+"/"+max_i+"  Cost= "+x);
				
						if (solverP.probSelectLocMin <= 100 && j == max_i)
							continue;
				
						//
						if (x < listNewCost(th)){
							listIndexJNb(th) = 1;
							listNewCost(th) = x;
							minth(th) = j;
							//if (solverP.firstBest)
							//{
								//return lmin_j;         
							//}
						} else if (x == listNewCost(th)){
							listIndexJNb(th) = listIndexJNb(th) + 1;
							if (randomth.nextInt(listIndexJNb(th)) == 0)
								minth(th) = j; 
						}
					}
				}
			}
			var thWin : Int = 0;
			if (listNewCost(1)<listNewCost(2)){
				new_cost = listNewCost(1);
				thWin=1;
				list_j_nb = listIndexJNb(1);
			}else if(listNewCost(2)<listNewCost(1)){
				new_cost = listNewCost(2);
				thWin=2;
				list_j_nb = listIndexJNb(2);
			}else{
				new_cost = listNewCost(1);
				list_j_nb = listIndexJNb(1)+listIndexJNb(2);
				thWin = random.randomInt(2)+1;
			}
			lmin_j = minth(thWin);
			
			if (solverP.probSelectLocMin <= 100)
			{
				if (new_cost >= total_cost && 
						(random.randomInt(100) < solverP.probSelectLocMin ||(list_i_nb <= 1 && list_j_nb <= 1)))
				{
					lmin_j = max_i;
					return lmin_j;
				}
				
				if (list_j_nb == 0)
				{
					nbIter++;
					val ranIndex = random.randomInt(list_i_nb);
					max_i = list_i(ranIndex);
					flagOut = true; 
				}
			}
		}while(flagOut);
		
		return lmin_j;
	}*/
}

class ThreadData{
	val listIth : Rail[Int]; 
	var nbIndex : Int;
	var cost : Int;
	var varMarked : Int;
	
	def this( part : Int ){
		listIth = new Rail[Int](0..part,0); 
		nbIndex = 0;
		cost = 0;
		varMarked = 0;
	}
	
}