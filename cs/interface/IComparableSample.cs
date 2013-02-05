/// <summary>
///   IComparable のサンプル
/// </summary>
///
/// コンパイル(IComparable 版)    : csc IComparableSample.cs Point_IComparable.cs
/// コンパイル(IComparable<T> 版) : csc IComparableSample.cs Point_IComparable_T.cs

using System;
using System.Collections.Generic;

using Geometry;

namespace IComparableSample
{
    
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
	    Console.WriteLine("## Binary Search ##");
	    var target = new Point(3, 1);
	    int pos = Array.BinarySearch(ary, target);
	    Console.WriteLine("{0} @ {1}", target, pos);
        }
    }
}
