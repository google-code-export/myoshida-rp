//
// ICloneable のサンプル
//
// コンパイル : csc ICloneableSample.cs Point.cs

using System;

using Geometry;


namespace ICloneableSample
{

    class Program
    {
	static void Main(string[] args)
	{
	    Point a = new Point(1, 2);
	    Point b = a;
	    Point c = (Point)a.Clone();

	    Console.WriteLine("==== Base ====");
	    Console.WriteLine("a = {0}", a);
	    Console.WriteLine("b = {0}", b);
	    Console.WriteLine("c = {0}", c);
	    Console.WriteLine();

	    a.x = 5;
	    Console.WriteLine("==== Changed A (a.x = 5) ====");
	    Console.WriteLine("a = {0}", a);
	    Console.WriteLine("b = {0}", b);      
	    Console.WriteLine("c = {0}", c);      
	}
    }  
}


