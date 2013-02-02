/// <summary>
///   IComparable のサンプル
/// </summary>
///
/// コンパイル : csc IComparableSample.cs Point.cs

using System;
using System.Collections.Generic;


namespace IComparableSample
{
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
	}
	
        static void Main(string[] args)
        {
	    // Point を格納したデータを準備
	    Point [] ary = {
		new Point(1, 2),
		new Point(3, 5),
		new Point(1, 9),
		new Point(2, 8),
		new Point(4, 1),
		new Point(3, 1)
	    };	    
	    DumpArray("Original", ary);

	    // データのソート
	    Array.Sort(ary);

	    // ソート結果を出力
	    DumpArray("Sorted", ary);
        }
    }
}
