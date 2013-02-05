//
// IEnumerable のサンプル
//
// コンパイル : csc IEnumerableSample.cs Point.cs

using System;
using System.Collections;

using Geometry;


namespace IEnumerableSample
{

    class Polygon : IEnumerable
    {
	private Point[] _points;

	public Polygon(Point[] points)
	{
	    if (points != null)
		_points = (Point[])points.Clone();
	}

	public int Count
	{
	    get { return _points.Length; }
	}

	/// 列挙子を取得
	public IEnumerator GetEnumerator()
	{
	    return _points.GetEnumerator();
	}    


	public Point this[uint idx]
	{
	    get {
		return _points[idx];
	    }
	    set {
		_points[idx] = value;
	    }
	}
    }


    class Program
    {
	static void Main()
	{
	    Point[] points = new Point[] {
		new Point(0, 0),
		new Point(5, 0),
		new Point(0, 5)
	    };
	    Polygon poly = new Polygon(points);
	    
	    Console.Write("Polygon = {\n  ");
	    foreach (Point pos in poly)
	    {
		Console.Write("{0} ", pos);
	    }
	    Console.WriteLine("\n}");
	}
    }

}
