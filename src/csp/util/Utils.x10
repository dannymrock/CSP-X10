/* 
 * COP-X10: An X10 Implementation of the CPMH framework
 * 
 * MIT License
 *
 * Copyright (c) 2022 Danny Munera
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package csp.util;

public class Utils {
	public static def copy[T](a:Rail[T]):Rail[T]{self.size==a.size} =new Rail[T](a.size, (i:Long)=>a(i));
	
	public static def show(s:String, d: Rail[Int]) {
	    Console.OUT.print(s + " in "+here.id+" : ");
	    for(k in d.range()) 
	        Console.OUT.print(" " + d(k));		
	    Console.OUT.println("");
	}
}
