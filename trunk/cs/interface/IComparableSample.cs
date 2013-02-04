/// <summary>
///   IComparable のサンプル
/// </summary>
///
/// コンパイル : csc IComparableSample.cs

using System;
using System.Collections.Generic;


namespace IComparableSample
{
    class Point : IComparable
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
	public int CompareTo(Object obj)
	{
	    if (obj == null) return 1;
	     
	    Point other = (Point)obj;
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
	    DumpArray("Original", ary);

	    // データのソート
	    Array.Sort(ary);
	    DumpArray("Sorted", ary);

	    // 二分探索
	    Console.WriteLine("==== Binary Search ====");
	    var target = new Point(3, 1);
	    int pos = Array.BinarySearch(ary, target);
	    Console.WriteLine("{0} => {1}", target, pos);
        }
    }
}
