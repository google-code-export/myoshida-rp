/// <summary>
///   IComparable のサンプル
/// </summary>
///
/// コンパイル : csc IComparableSample.cs Point.cs

using System;


namespace IComparableSample
{
    class Program
    {
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

	    Console.WriteLine("==== Original ====");
	    foreach (Point pos in ary)
	    {
		Console.Write("{0} ", pos);
	    }
	    Console.WriteLine();

	    // データのソート
	    Array.Sort(ary);

	    // ソート結果を出力
	    Console.WriteLine("==== Sorted ====");
	    foreach (Point pos in ary)
	    {
		Console.Write("{0} ", pos);
	    }
	    Console.WriteLine();	    
        }
    }
}
