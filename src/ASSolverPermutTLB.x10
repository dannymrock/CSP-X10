/** ASSolverPermutTLB is the implementation of Adaptive Search solver
 * 	in the x10 lenguage with Thread Level Parallelism.
 *  Implementation specialized in Permuts Problems.
 * 
 *  Based on the C implementation of Adaptive Search algoritm by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013 -> first version
 * 				 	12 April, 2013 -> Fix some bugs with TLP implementation
 * 	
 */

import x10.util.Random;
public class ASSolverPermutTLB extends ASSolverPermut { 
	val computeInst : Array[ComputePlace];
	var startBarrier : ThreadBarrier;
	var doneBarrier : ThreadBarrier;
	val nbThread : Int;
	var thIndex : Int; 
	/**
	 *  Constructor of the class
	 * 	@param sizeOfProblem size of the problem to solve
	 *  @seed seed for the randomness in the object.
	 * 
	 */
	public def this( sizeOfProblem : Int , seed : Long, updateI : Int) {
		super(sizeOfProblem, seed, updateI);
		
		//Maximum number of threads supported = 2
		nbThread = 2;
		computeInst = new Array[ComputePlace](0..(nbThread-1));		
		startBarrier = new ThreadBarrier(nbThread+1); //nbthread workers threads + 1 master thread
		doneBarrier = new ThreadBarrier(nbThread+1);
	}

	/**
	 *  solve( csp : ModelAS ) : Int
	 *  Solve a csp Problem through the Adaptive Search algoritm
	 * 	@param csp The model of the problem to solve
	 *  @return the final total cost after solving process (If success returns 0)
	 */ 
	public def solve( csp : ModelAS) : Int {
		
		for(var p : Int = 0 ; p < nbThread; p++){
			computeInst(p) = new ComputePlace(p , csp);
		}
		for(id in computeInst) async computeInst(id).run();
		
		val cost = super.solve(csp);

		for(id in computeInst) 
			computeInst(id).activity = 1;
		
		//startSignal.advance(); // send start signal
		startBarrier.wait();
		doneBarrier.wait();
		return cost;
	}
	
	
	public def selectVarHighCost(csp : ModelAS) : Int {
				
		for(id in computeInst) computeInst(id).activity = 0;
		startBarrier.wait(); // send start signal
		doneBarrier.wait(); // work ready
		
		
		//val maxI = terminateSelVarHighCost();
		//nb_var_marked = 0;
		nb_var_marked = computeInst(0).localnbVarMarked + computeInst(1).localnbVarMarked;
		thIndex = 0;
		
		if (computeInst(1).maxCost  >  computeInst(0).maxCost) {
			thIndex =  1;
		} else if (computeInst(1).maxCost < computeInst(0).maxCost){
			thIndex =  0;
		} else if (computeInst(1).maxCost == computeInst(0).maxCost){
			list_i_nb = computeInst(0).list_i_index + computeInst(1).list_i_index;
			Array.copy(computeInst(0).threadList_i , 0 , list_i , 0 , computeInst(0).list_i_index);
			Array.copy(computeInst(1).threadList_i , 0 , list_i , computeInst(0).list_i_index , computeInst(1).list_i_index);
			nbSameVar += list_i_nb;
			//return computeInst(0).max_i_th;
			val x = random.randomInt(list_i_nb);
			//Console.OUT.println("list_i_nb "+list_i_nb+ " x "+x+" list_i(x) "+list_i(x));
			return list_i(x);
		}
		list_i_nb = computeInst(thIndex).list_i_index;
		Array.copy(computeInst(thIndex).threadList_i , 0 , list_i, 0 , computeInst(thIndex).list_i_index);
		nbSameVar += list_i_nb;
		val x = random.randomInt(list_i_nb);
		//Console.OUT.println("list_i_nb "+list_i_nb+ " x "+x+" list_i(x) "+list_i(x));
		return list_i(x);
		
		//return computeInst(thIndex).max_i_th;
	}
	
	class ComputePlace{
		var activity:Int = 0; 
		val idI:Int;
		var terminate:Boolean;
		
		var list_i_index:Int;
		val partition:Int;
		val csp:ModelAS;
		val threadList_i : Array[Int];
		//var max_i_th:Int;
		var maxCost: Int;
		val r = new Random();
		var localnbVarMarked:Int;
		
		
		def this( id : Int, cspIn : ModelAS){
			idI = id;
			terminate = false;
			//partition = size/idI;//esto esta malo?
			partition = size/nbThread;
			//Console.OUT.println("partition: "+partition);
			csp = cspIn;
			threadList_i = new Array[Int](0..(partition - 1),0);
			localnbVarMarked = 0;
		}
		
		def run(){
			
			while(!terminate){
				
				startBarrier.wait();
				
				switch(activity){ 
				case 0:
					threadSelectVarHighCost();
					//Console.OUT.println("Worker: "+idI+" Do Nothing...");
					break;
				case 1:
					terminate = true;
					
				}
				//Console.OUT.println("Master is processing: "+idI+" Ready...");
				doneBarrier.wait();
			}
			
			return;
		}
		
		
		def threadSelectVarHighCost(){
			var i: Int;
			var x: Int;
			
			
			//max_i_th = -1;
			list_i_index = 0;
			maxCost = -1;
			localnbVarMarked = 0;
			
			for (i = idI * partition ; i < (idI+1) * partition ; i++)		//while(i++ < size) 
			{	
				//Console.OUT.print("    Th "+idI+" i "+i+"   -" );
				if (nbSwap < mark(i))
				{
					localnbVarMarked++;
					continue;
				}
				
				x = csp.costOnVariable(i);
				
				if (x >= maxCost){
					if (x > maxCost){
						maxCost = x;
						list_i_index = 0;
					}
					threadList_i(list_i_index++) = i; 
				}
			}
			//nbSameVar += list_i_nb;
		}//end selecVarHighCost
	}//end ComputePlace inner class
}