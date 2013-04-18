public class CommData {
	var bestCost : Int;
	var bestPlaceId : Int;
	val costArray : Array[Int];
	def this(bestcost : Int, bestP : Int ){
		bestCost = bestcost;
		bestPlaceId = bestP;
		costArray = new Array[Int](0..(Place.MAX_PLACES),-1);
	}
}