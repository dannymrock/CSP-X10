package csp.solver;
import csp.model.ParamManager;
import x10.array.Array_2;
import csp.model.ModelAS;
import csp.util.Utils;

public class RoTSearch extends RandomSearch {
	 
	 private val tabuDurationFactor : Double = 8.0;
	 private val aspirationFactor : Double = 5.0;
	 private val tabuDuration : Int;
	 private val aspiration : Int;
	 
	 private var autorized : Boolean; 
	 private var aspired : Boolean;
	 private var alreadyAspired : Boolean;
	 
	 
	 /** Tabu List Matrix */
	 private val tabuList : Array_2[Long];
	 
	 public def this(sizeS:Long, solver:IParallelSolver(sizeS), opts:ParamManager)
	 : RoTSearch(sizeS){
		  super(sizeS, solver, opts);
		  
		  this.tabuDuration = (tabuDurationFactor * this.sz) as Int;
		  this.aspiration = (aspirationFactor * this.sz * this.sz) as Int;
		  
		  this.tabuList = new Array_2 [Long](this.sz, this.sz , 0);
		  
	 }
	 
	 protected def initVar( cop_:ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  super.initVar(cop_, tCost, sLow);
		  
		  for (var i:Long = 0 ; i < this.sz; i++)
				for (var j:Long = 0 ; j < this.sz; j++)
					 this.tabuList(i,j) = -(this.sz * i + j);

	 }
	 
	 
	 protected def search( cop_ : ModelAS{self.sz==this.sz}) : Long{
		  var i : Long;
		  var j : Long;
		  
		  var newCost : Long;
		  var delta : Long;
		  var minDelta : Long = Long.MAX_VALUE;
		  move.setFirst(Long.MAX_VALUE);
		  move.setSecond(Long.MAX_VALUE);
		  this.alreadyAspired = false;
		  
		  //Utils.show("Solution",cop_.getVariables());
		  
		  for (i = 0; i < this.sz - 1; i++)
				for (j = i + 1; j < this.sz; j++) {
					 
					 newCost = cop_.costIfSwap(this.currentCost,i,j);
					 delta = newCost - this.currentCost;
					 
					 this.autorized =
						   ( tabuList (i,cop_.variables(j)) < this.nIter) ||
						   ( tabuList (j,cop_.variables(i)) < this.nIter);
					  
					 this.aspired =
						   ( tabuList(i,cop_.variables(j)) < this.nIter - this.aspiration) ||
						   ( tabuList(j,cop_.variables(i)) < this.nIter - this.aspiration) ||
						   ( newCost < this.bestCost);
					 
					 if ((aspired && !alreadyAspired) ||	/* first move aspired */
						  (aspired && alreadyAspired &&	/* many move aspired */
						  (delta <= minDelta)) ||	/* => take best one */
						  (!aspired && !alreadyAspired &&	/* no move aspired yet */
						  (delta <= minDelta) && autorized)) {

						  //   #ifdef USE_RANDOM_ON_BEST
						  //   if (delta[i][j] == min_delta){
						  // 		if (Random(++best_nb) > 0)
						  // 			 continue;
						  //   }
						  //   else
						  // 		best_nb = 1;
						  //   #endif
						  
						  move.setFirst(i);
						  move.setSecond(j);
						  minDelta = delta;
						  
						  // #ifdef FIRST_BEST
						  // if (current_cost + min_delta < best_cost)
						  // goto found;
						  // #endif
						  
						  if (aspired)
								alreadyAspired = true;
					  }
				}
		  
		  
		  if(move.getFirst() == Long.MAX_VALUE){
				Console.OUT.println("All moves are tabu! \n");
				return currentCost;
		  }
		  else {
				//Console.OUT.println("swap pos "+move.getFirst()+" "+move.getSecond());
				
				cop_.swapVariables(move.getFirst(), move.getSecond()); //adSwap(maxI, minJ,csp);	
				nSwap++;
				cop_.executedSwap(move.getFirst(), move.getSecond());
				
				/* forbid reverse move for a random number of iterations */
				val ran1 = random.nextDouble();
				
				tabuList( move.getFirst(), cop_.variables(move.getSecond())) = this.nIter + ((ran1*ran1*ran1) * this.tabuDuration) as Int;
				val ran2 = random.nextDouble();
				tabuList( move.getSecond(), cop_.variables(move.getFirst())) = this.nIter + ((ran2*ran2*ran2) * this.tabuDuration) as Int;
					 //current_iteration + (int) (cube(Random_Double()) * tabu_duration);
				//Utils.show("after swap",cop_.getVariables());
				return this.currentCost + minDelta;
		  }
		  
	 }
	 
}
public type RoTSearch(s:Long)=RoTSearch{self.sz==s};