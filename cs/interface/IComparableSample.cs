/// <summary>
///   IComparable のサンプル
/// </summary>
///
/// コンパイル(IComparable 版)    : csc IComparableSample.cs Point_IComparable.cs
/// コンパイル(IComparable<T> 版) : csc IComparableSample.cs Point.cs

using System;
using System.Collections.Generic;
using Geometry;

namespace IComparableSample
{
    
    class Program
    {
	static void WriteTitle(string title)
	{
	    Console.WriteLine();
	    Console.WriteLine(title);
	    Console.WriteLine(new String('=', title.Length));
	    Console.WriteLine();
	}
	
	static void DumpArray<Type>(string title, IEnumerable<Type> ary)
	{	    
	    Console.WriteLine("## " + title + " ##\n");
	    Console.Write("    ");
	    foreach(var it in ary)
	    {
	    	Console.Write("{0} ", it);
	    }
	    Console.WriteLine();
	    Console.WriteLine();
	}
	
        static void Main()
        {
	    // 配列のソートと検索
	    WriteTitle("Array");
	    
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
	    Console.WriteLine("## Binary Search ##\n  ");
	    var target = new Point(3, 1);
	    int pos = Array.BinarySearch(ary, target);
	    Console.WriteLine("    {0} => {1}", target, pos);


	    Console.WriteLine("\n");
	    // SortedList (Key, Value のペアーを Key でソートして格納するコンテナー)
	    WriteTitle("SortedList");

	    // Point を格納したデータを準備
	    SortedList<Point, string> alist = new SortedList<Point, string>();
	    Point p = null;
	    p = new Point(1, 2); alist.Add(p, "A"); Console.WriteLine("    Add {0}, A", p);
	    p = new Point(3, 5); alist.Add(p, "B"); Console.WriteLine("    Add {0}, B", p);
	    p = new Point(1, 9); alist.Add(p, "C"); Console.WriteLine("    Add {0}, C", p);
	    p = new Point(4, 1); alist.Add(p, "D"); Console.WriteLine("    Add {0}, D", p);
	    p = new Point(3, 1); alist.Add(p, "E"); Console.WriteLine("    Add {0}, E", p);

	    Console.WriteLine("\n## SortedList Contents ##\n");
	    foreach( KeyValuePair<Point, string> kvp in alist )
	    {
	    	Console.WriteLine("    {0} : {1}", kvp.Key, kvp.Value);
	    }

        }
    }
}
