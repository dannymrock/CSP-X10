//package fr.univ.paris1.cri.CSPSolver.tools;
import x10.util.Random; 

public class RandomTools { 
	var seed: Long; 
	val r : Random;
	
	public def this(val s: Long){
		 seed=s;
		 r = new Random();
		 r.setSeed(s);
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
	
	public def randomInt(var limit : Int):Int{
		return(r.nextInt(limit));		
	}
	
	public def randomLong():Long{
		return(r.nextLong());		
	}
	
	randomDouble():Double{
		return r.nextDouble();
	}
}