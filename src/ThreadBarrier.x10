import x10.util.concurrent.Monitor;
public class ThreadBarrier {
	val monitor: Monitor;
	var left : Int;
	val initCount : Int;
	var currEvent : Int;
	
	def this(nthreads : Int){
		monitor = new Monitor();
		left = nthreads;
		initCount = nthreads;
	}
	
	public def wait()
	{
		monitor.lock();
		if (--left == 0) {
			monitor.release();
		} else {
			monitor.await();
			monitor.unlock();
		}
		atomic{ left++;}
	}	
	
}