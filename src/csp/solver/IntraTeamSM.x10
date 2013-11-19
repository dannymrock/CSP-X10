package csp.solver;
/**
 *  IntraTeamSM
 * 
 * 	This class contains all the components to implement the Intra-Team communication
 *  between the solver into a Team. The object of this Class will be Shared by all the 
 *  solver (activities) into the team (a Place) (Shared Memory Model) 
 */

import csp.utils.*;

public class IntraTeamSM {
	
	val monitor = new MonitorV ("IntraTeamSM");
	
	/** Elite Pool
	 */
	val elitePool : ElitePool;
	
	public def this(poolSize:Int){
		elitePool =  new ElitePool(poolSize);
	}
	
	public def insertConfToPool(info:CSPSharedUnit){
		monitor.atomicBlock(()=>{elitePool.tryInsertVector(info.cost,info.conf,info.place);Unit()});
	}
	
	public def getConfFromPool():CSPSharedUnit{
		return monitor.atomicBlock(()=>elitePool.getRemoteData());		
	}
	
}