//
// ICloneable のサンプル
//
// コンパイル : csc ICloneableSample.cs Point.cs

using System;
using System.Collections.Generic;
using System.Linq;

using Geometry;


namespace ICloneableSample
{
    class Segment : ICloneable
    {
	public Point BeginPoint { set; get; }
	public Point EndPoint { set; get; }

	public Segment(int x1, int y1, int x2, int y2)
	{
	    BeginPoint = new Point(x1, y1);
	    EndPoint = new Point(x2, y2);
	}

	public object Clone()
	{	    
	    return new Segment(BeginPoint.x, BeginPoint.y,
			       EndPoint.x, EndPoint.y);
	}
	public override string ToString()
	{
	    return BeginPoint.ToString() + " - " + EndPoint.ToString();
	}	
    }

    class Program
    {
	static void Main(string[] args)
	{
	    Point a = new Point(1, 2);
	    Point b = a;
	    Point c = (Point)a.Clone();

	    Console.WriteLine("## Base ##");
	    Console.WriteLine("a = {0}", a);
	    Console.WriteLine("b = {0}", b);
	    Console.WriteLine("c = {0}", c);
	    Console.WriteLine();

	    a.x = 5;
	    Console.WriteLine("## Changed A (a.x = 5) ##");
	    Console.WriteLine("a = {0}", a);
	    Console.WriteLine("b = {0}", b);
	    Console.WriteLine("c = {0}", c);

	    
	    Console.WriteLine("\n================\n");
	    
	    Segment aseg = new Segment(0, 0, 2, 2);
	    Segment bseg = (Segment)aseg.Clone();
	    Point[] apoints = { new Point(0,0), new Point(2,2) };
	    Point[] bpoints = (Point[])apoints.Clone();

	    Console.WriteLine("## Base ##");
	    Console.WriteLine("aseg = {0}", a);
	    Console.WriteLine("bseg = {0}", b);
	    Console.WriteLine("apoints = {0}, {1}", apoints[0], apoints[1]);
	    Console.WriteLine("bpoints = {0}, {1}", bpoints[0], bpoints[1]);
	    Console.WriteLine();
	    
	    Console.WriteLine("## Changed A ([1].x = 5) ##");
	    aseg.EndPoint.x = 5;
	    apoints[1].x = 5;
	    Console.WriteLine("aseg = {0}", aseg);
	    Console.WriteLine("bseg = {0}", bseg);
	    Console.WriteLine("apoints = {0}, {1}", apoints[0], apoints[1]);
	    Console.WriteLine("bpoints = {0}, {1}", bpoints[0], bpoints[1]);
	}
    }  
}


