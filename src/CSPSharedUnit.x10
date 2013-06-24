struct CSPSharedUnit {
	val cost : Int;
	val vector : Rail[Int];
	val place : Int;
	def this( costI : Int, sizeI : Int, vectorI : Rail[Int], placeI : Int){
		cost = costI;
		vector = new Rail[Int](0..(sizeI-1));
		Array.copy(vectorI, vector);
		place = placeI;
	}
}