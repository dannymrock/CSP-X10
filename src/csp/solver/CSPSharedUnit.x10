package csp.solver;

struct CSPSharedUnit {
	val cost : Int;
	val conf : Rail[Int];
	val place : Int;
	def this( costI : Int, sizeI : Int, confI : Rail[Int], placeI : Int){
		cost = costI;
		conf = new Rail[Int](sizeI);
		Rail.copy(confI, confI);
		place = placeI;
	}
}