﻿/// <summary>
///   ジェネリックな点クラスの IComparable のサンプル
/// </summary>
///
/// コンパイル : csc GenericsPoint.cs

using System;
using System.Collections.Generic;


namespace GenericsPointSample
{
    class Point<Type> : IComparable<Point<Type> >
	where Type : IComparable
    {
	/// <summary>
	///   x 座標
	/// </summary>
	public Type x  { get; set; }

	/// <summary>
	///   y 座標
	/// </summary>
	public Type y  { get; set; }


	/// <summary>
	/// 座標値 (x, y) を与えて初期化。
	/// </summary>
	/// <param name="x">x 座標値</param>
	/// <param name="y">y 座標値</param>
	public Point(Type x, Type y)
	{
	    this.x = x;
	    this.y = y;
	}


	/// <summary>
	///   比較メソッド
	/// </summary>
	public int CompareTo(Point<Type> other)
	{
	    if (other == null) return 1;
	    
	    if (x.CompareTo(other.x) == 0) {
		return y.CompareTo(other.y);
	    }	
	    return x.CompareTo(other.x);
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
	    Console.WriteLine("## " + title + " ##");
	    foreach(var it in ary)
	    {
	    	Console.Write("{0} ", it);
	    }
	    Console.WriteLine("\n");
	}
	
        static void Main()
        {
	    // Point を格納したデータを準備
	    Point<int> [] ary = {
		new Point<int>(1, 2),
		new Point<int>(3, 5),
		new Point<int>(1, 9),
		new Point<int>(4, 1),
		new Point<int>(3, 1)
	    };	    
	    DumpArray("Original", ary);

	    // データのソート
	    Array.Sort(ary);
	    DumpArray("Sorted", ary);

	    // 二分探索
	    Console.WriteLine("## Binary Search ##");
	    var target = new Point<int>(3, 1);
	    int pos = Array.BinarySearch(ary, target);
	    Console.WriteLine("{0} @ {1}", target, pos);
        }
    }
}
