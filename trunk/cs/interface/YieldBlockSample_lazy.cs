//
// yield ブロックのサンプル(遅延処理の確認)
//
// コンパイル : csc YieldBlockSample_lazy.cs Point.cs

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
	    {
		_points = new Point[points.Length];
		for (int cnt = 0; cnt < points.Length ; cnt++)
		{
		    _points[cnt] = new Point(points[cnt]);
		}	
	    }	
	}

	public Point this[int idx]
	{
	    get { return _points[idx]; }
	    set { _points[idx] = value; }
	}
	
	/// <summary>
	/// 列挙子を取得
	/// </summary>
	public IEnumerator GetEnumerator()
	{
	    foreach (Point pos in _points)
	    {
		yield return pos;
	    }
	}

	/// <summary>
	///   閉じた折れ線の取得
	/// </summary>
	public IEnumerable ClosedPolyline()
	{
	    foreach (Point pos in _points)
	    {
		yield return new Point(pos);
	    }
	    yield return new Point(_points[0]);
	}
	
    }

    class Program
    {
	static void Main()
	{
	    Point[] points = {
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
	    

	    // 遅延処理の確認
	    var polyline = poly.ClosedPolyline();
	    Console.WriteLine("polyline = {0}", polyline);
	    poly[1].x = 2;
	    Console.WriteLine("Change poly[1].x = {0}", poly[1].x);

	    Console.Write("Closed Polyline = {\n  ");
	    foreach (Point pos in polyline)
	    {
		Console.Write("{0} ", pos);
	    }
	    Console.WriteLine("\n}");
	}
    }

}
