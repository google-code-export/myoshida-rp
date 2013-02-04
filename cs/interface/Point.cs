/// <summary>
///   点クラス (IComparable のサンプル用)
/// </summary>

using System;
using System.Collections.Generic;

namespace Geometry
{
    class Point : IComparable, IComparable<Point>
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
	///   比較メソッド
	/// </summary>
	public int CompareTo(Object obj)
	{
	    return CompareTo((Point)obj);
	}


	/// <summary>
	///   文字列化メソッド
	/// </summary>
	public override string ToString()
	{
	    return "(" + x + ", " + y + ")";
	}
	
    } // Point


    class PointTest
    {
	static void DumpArray<Type>(string title, IEnumerable<Type> ary)
	{	    
	    Console.WriteLine("==== " + title + " ====");
	    foreach(var it in ary)
	    {
	    	Console.Write("{0} ", it);
	    }
	    Console.WriteLine();
	    Console.WriteLine();
	}
	
        static void Main()
        {
	    // Point を格納したデータを準備
	    Point [] ary = {
		new Point(1, 2),
		new Point(3, 5),
		new Point(1, 9),
		new Point(4, 1),
		new Point(3, 1)
	    };
	    DumpArray("Original (Array)", ary);
	    List<Point> ls = new List<Point>(ary);
	    

	    // データのソート
	    Array.Sort(ary);
	    DumpArray("Sorted (Array)", ary);	    
	    

	    DumpArray("Original (List)", ls);

	    // データのソート
	    ls.Sort();
	    DumpArray("Sorted (List)", ls);
        }
    }

}
