/** RandomTools 
 * 	This class has some random tools
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 10, 2013  First Version
 */
import x10.util.Random;

public class RandomTools { 
	var seed: Long; 
	val r : Random;
	
	public def this(val seedIn: Long){
		 seed=seedIn;
		 r = new Random();
		 r.setSeed(seedIn);
	} 
	
	public def setSeed(s:Long){
		r.setSeed(s);
		seed=s;
	}
	
	public def randomPermut( size : Int , baseValue : Int ) : Array[Int](1)
	{
		var i:Int; 
		var j:Int;
		var z:Int;
		val vec : Array[Int](1) = new Array[Int](0..(size - 1), ([k]:Point) => (baseValue + k));
				
		for(i = size - 1; i > 0 ; i--)
		{
			j = r.nextInt( i + 1 );
			z = vec(i);
			vec(i) = vec(j);
			vec(j) = z;
		}
		return vec;		
	}
	
	/**
	 * 	randomArrayPermut
	 * 	Generate a random permutation of a given vector of size elements
	 * 	@param vec Vector with initial values
	 * 	@param size 
	 * 	@return
	 */
	public def randomArrayPermut( vec : Array[Int] ) : Array[Int]{
		var i:Int; 
		var j:Int;
		var z:Int;
		val vSize = vec.size;
		
		for(i = vSize - 1; i > 0 ; i--)
		{
			j = r.nextInt( i + 1 );
			z = vec(i);
			vec(i) = vec(j);
			vec(j) = z;
		}
		return vec;	
	}
	
	
	public def randomInt(var limit : Int):Int{
		return(r.nextInt(limit));		
	}
	
	public def randomLong():Long{
		return(r.nextLong());		
	}
	
	public def randomDouble():Double{
		return r.nextDouble();
	}
}