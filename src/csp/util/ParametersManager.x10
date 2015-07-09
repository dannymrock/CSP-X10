package csp.util;
import x10.util.HashMap;
import x10.util.OptionsParser;
import x10.util.Option;

/**
 * Class ParametersManager
 * 
 * This class manages all the parameters of the Program
 * 
 */



public class ParametersManager extends OptionsParser {

	 public def this(args: Rail[String]) {
		  super(args, 
					 [Option("h", "help", "shows this help message and exit")],
					 [Option("p", "", "(p)roblem to solve"),
					  Option("f", "", "(f)ile path for SMTI or QAP"),
					  Option("s", "", "(S)ize of the problem"),
					  Option("m", "", "Solver (m)ode distribution 0 for Places \"n\" for Activities (n number of activities). Default 0."),
					  Option("l", "", "restart (l)imit"),
					  Option("t", "", "(T)ime out default 0"),
					  Option("c", "", "target (c)ost from Command Line Parameter. default 0"),
					  Option("a", "", "Flag to receive target cost form file. default 0 from command line, 1 take optimal from file, 2 take BKS from file "),
					  Option("b", "", "Number of (b)enchmark tests"),
					  Option("sol", "", "Solver to use"),
					  Option("N", "", "nodes_per_team parameter. Default 4."),
					  Option("U", "", "Update Interval Intra-team Communication (iterations) . Default 0 - no communication."),
					  Option("R", "", "Report Interval Intra-team Communication (iterations) . Default 0 - no communication."),
					  Option("C", "", "Probability to change vector in Intra-Team Communication "),
					  Option("P", "", "poolsize."),
					  Option("I", "", "Inter-team Communication Interval (miliseconds) . Default 0 - no communication."),
					  Option("D", "", "minimum permisible distance."),
					  Option("W", "", "initial (W)ait  before start Inter-team Communication (miliseconds). Default 0"),
					  Option("A", "", "Inter Team Communicaction Diversification - Percentage of Places (A)ffected . Default 0."),
					  Option("y", "", "seed. Default 0"),
					  Option("v", "", "verify and print solution. Default 0"),
					  Option("i", "", "file path for input vector . Default ."),
					  Option("o", "", "output format: csv 0, info 1")]);
		  
	 }

}