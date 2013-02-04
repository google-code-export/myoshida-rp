/// <summary>
///   ジェネリックな IComparable のサンプル
/// </summary>
///
/// コンパイル : csc IComparableTypeSample.cs

using System;
using System.Collections.Generic;

namespace IComparableTypeSample
{
    class Point : IComparable<Point>
    {
	/// <summary>
	///   x 座標
	/// </summary>
	public int x  { get; set; }

	/// <summary>
	///   y 座標
	/// </summary>
	public int y  { get; set; }


	/// <summary>
	/// 座標値 (x, y) を与えて初期化。
	/// </summary>
	/// <param name="x">x 座標値</param>
	/// <param name="y">y 座標値</param>
	public Point(int x, int y)
	{
	    this.x = x;
	    this.y = y;
	}


	/// <summary>
	///   比較メソッド
	/// </summary>
	public int CompareTo(Point other)
	{
	    if (other == null) return 1;
	     
	    if (other.x == x) {
		return y - other.y;
	    }	
	    return x - other.x;
	}


	/// <summary>
	///   文字列化メソッド
	/// </summary>
	public override string ToString()
	{
	    return "(" + x + ", " + y + ")";
	}
	
    } // Point


    class Program
    {
	static void WritePointPairLine(KeyValuePair<Point, string> kvp, string prefix = "")
	{
	    Console.WriteLine("{0} {1} : {2}", prefix, kvp.Key, kvp.Value);
	}
	
        static void Main()
        {
	    // Point を格納したデータを準備
	    Console.WriteLine("==== Create SortedList ====");
	    SortedList<Point, string> alist = new SortedList<Point, string>();
	    Point p = null;
	    p = new Point(1, 2); alist.Add(p, "A"); Console.WriteLine("Add {0}, A", p);
	    p = new Point(3, 5); alist.Add(p, "B"); Console.WriteLine("Add {0}, B", p);
	    p = new Point(1, 9); alist.Add(p, "C"); Console.WriteLine("Add {0}, C", p);
	    p = new Point(4, 1); alist.Add(p, "D"); Console.WriteLine("Add {0}, D", p);
	    p = new Point(3, 1); alist.Add(p, "E"); Console.WriteLine("Add {0}, E", p);

	    Console.WriteLine("\n==== SortedList ====");
	    foreach( KeyValuePair<Point, string> kvp in alist )
	    {
	    	Console.WriteLine("{0} : {1}", kvp.Key, kvp.Value);
	    }
	
	}

    } // Program
    
}


