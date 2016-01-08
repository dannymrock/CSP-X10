package csp.solver;
import csp.model.ParamManager;
import x10.array.Array_2;
import csp.model.ModelAS;
import csp.util.Utils;
import csp.model.Main;

public class RoTSearch extends RandomSearch {
	 
	 private val tabuDurationFactorUS : Double;
	 private val aspirationFactorUS : Double;
	 private var tabuDurationFactor : Double;
	 private var aspirationFactor : Double;
	 private var tabuDuration : Int;
	 private var aspiration : Int;
	 
	 private var autorized : Boolean; 
	 private var aspired : Boolean;
	 private var alreadyAspired : Boolean;
	 
	 /** Tabu List Matrix */
	 private val tabuList : Array_2[Long];
	 
	 /** Range for random factors 	  */
	 val tdd = 6.0;
	 val tdu = 10.0;
	 
	 val ad = 2.0;
	 val au = 7.0;
	 
	 public def this(sizeS:Long, solver:IParallelSolver(sizeS), opts:ParamManager)
	 : RoTSearch(sizeS){
		  super(sizeS, solver, opts);
		  
		  this.mySolverType = Main.RoTS_SOL;
		  //Console.OUT.println(here+" RoTS");
		  
		  this.tabuDurationFactorUS = opts("--RoTS_tabu_duration", 8.0);
		  this.aspirationFactorUS = opts("--RoTS_aspiration", 5.0);
		  
		  this.tabuList = new Array_2 [Long](this.sz, this.sz , 0);
		  
		  if (here.id == 0){
				if ( this.tabuDurationFactorUS == -1.0 )
					 Console.OUT.println("Parameters RoTS: tabu duration = random("+tdd+","+tdu+") * "+this.sz);
				else
					 Console.OUT.println("Parameters RoTS: tabu duration = "+tabuDurationFactorUS+" * "+this.sz);			
				
				if ( this.aspirationFactorUS == -1.0 )
					 Console.OUT.println("                 aspiration = random("+ad+","+au+") * "+this.sz+"^2 ");
				else
					 Console.OUT.println("                 aspiration = "+aspirationFactorUS+" * "+this.sz+"^2 ");
		  }
	 }
	 
	 /**
	  *  Initialize variables of the solver
	  *  Executed once before the main solving loop
	  */
	 protected def initVar( cop_:ModelAS{self.sz==this.sz}, tCost : Long, sLow: Boolean){
		  super.initVar(cop_, tCost, sLow);
		  
		  if (this.tabuDurationFactorUS == -1.0) // Random initialitation of Tabu duration Factor 
				this.tabuDurationFactor = tdd + (tdu-tdd) * random.nextDouble();
		  else
				this.tabuDurationFactor = this.tabuDurationFactorUS;
		  
		  if (this.aspirationFactorUS == -1.0) // Random initialitation of Tabu duration Factor 
				this.aspirationFactor = ad + (au-ad) * random.nextDouble();
		  else
				this.aspirationFactor = this.aspirationFactorUS;
			
		  this.tabuDuration = (this.tabuDurationFactor * this.sz) as Int;
		  this.aspiration = (this.aspirationFactor * this.sz * this.sz) as Int;
		  
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
				
				// detect loc min
				if (minDelta >= 0)
					 onLocMin(cop_);
				
				return this.currentCost + minDelta;
		  }
		  
	 }
	 
	 
	 /**
	  *  Create RoTS Solver State array to be send to Pool
	  *  oeState(0) = solverType  
	  *  oeState(1) = RoTS tabu duration Factor * 100
	  *  oeState(2) = RoTS aspiration Factor * 100
	  */
	 protected def createSolverState( ) : Rail[Int]{self.size==3}{
		  val rotsState = new Rail[Int](3,-1n);
		  rotsState(0) = this.mySolverType;
		  rotsState(1) = (this.tabuDurationFactor * 10.0) as Int;
		  rotsState(2) = (this.aspirationFactor * 10.0) as Int; 
		  return rotsState;  
	 }
	 
	 /**
	  *  Process Solver State Array received from Pool
	  * 
	  */
	 protected def processSolverState( state : Rail[Int]{self.size==3}){
		  // Random Search has no parameters to process
		  
		  val inSolverType = state(0);
		  
		  if (inSolverType == this.mySolverType){
				val intdf = state(1)/ 10.0;
				val inaf = state(2) / 10.0;
				
				// this.tabuDurationFactor = intdf;
				// this.aspirationFactor = inaf;
				
				this.tabuDurationFactor = (this.tabuDurationFactor + intdf) / 2.0;
				this.aspirationFactor = (this.aspirationFactor + inaf) / 2.0;
							
				this.tabuDuration = (this.tabuDurationFactor * this.sz) as Int;
				this.aspiration = (this.aspirationFactor * this.sz * this.sz) as Int;
		  }
	 } 	 
	 
	 protected def restartVar(){
		  super.restartVar();
		  tabuList.clear();
	 }
	 
	 /**
	  *  Interact when Loc min is reached
	  */
	 private def onLocMin(cop : ModelAS){
		  // communicate Local Minimum
		  // solver.communicateLM( this.currentCost, cop.getVariables() as Valuation(sz));
		  val solverState = this.createSolverState();
		  this.solver.communicateLM( new CSPSharedUnit(sz, this.currentCost, cop.getVariables() as Valuation(sz), here.id as Int, solverState) );
	 }
	 
}
public type RoTSearch(s:Long)=RoTSearch{self.sz==s};
