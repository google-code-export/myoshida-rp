// コンパイル : csc StdQueryOperators.cs

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace LinqSample
{
    class StdQueryOperators
    {
	/// <summary>
	///   シーケンスのダンプメソッド
	/// </summary>
	static string Dump<TSource>(IEnumerable<TSource> source)
	{
	    return "{" + string.Join(", ", source) + "}";
	}

	//// 主要メソッド説明用
	static void MethodSamples()
	{

	    var src = new[] {3, 2, 9, 6};
	    Console.WriteLine("src           = {0}", Dump(src));

	    // Select (map)
	    var mapped = src.Select(elem => elem * 2); // {6, 4, 18, 12}
	    Console.WriteLine("map 'x 2'     = {0}", Dump(mapped));

	    // Where (filter)
	    var filtered = src.Where(elem => elem % 2 == 1); // {3, 9}
	    Console.WriteLine("fileter 'odd' = {0}", Dump(filtered));


	    // Aggregate
	    var sumval = src.Aggregate((sum, elem) => sum + elem); // 20
	    var maxval = src.Aggregate((max, elem) => (max < elem) ? elem : max); // 9
	    var cntval = src.Aggregate(0, (count, elem) => count+1); // 4
	    Console.WriteLine("sum = {0}", sumval);
	    Console.WriteLine("max = {0}", maxval);
	    Console.WriteLine("cnt = {0}", cntval);
	    
	    
	    // OrderBy (sort)
	    var possrc = new [] {
		    new { x = 1, y = 2 },
		    new { x = 3, y = 4 },
		    new { x = 1, y = 1 }
	    };
	    Console.WriteLine("src  = {0}", Dump(possrc));
	    var sorted = possrc.OrderBy(elem => elem.x).ThenBy(elem => elem.y);
	    // {{ x = 1, y = 1 }, { x = 1, y = 2 }, { x = 3, y = 4 }}
	    Console.WriteLine("sort = {0}", Dump(sorted));

	    // etc
	    Console.WriteLine("Count = {0}", 	src.Count());
	    Console.WriteLine("Take = {0}", 	Dump(src.Take(2)));
	    Console.WriteLine("First = {0}", 	src.First(elem => elem % 2 == 1));
	    Console.WriteLine("Contains = {0}",	src.Contains(9));
	    Console.WriteLine("Contains = {0}",	src.Contains(1));
	    Console.WriteLine("Min = {0}",	src.Min());
	    Console.WriteLine("Max = {0}",	src.Max());
	    Console.WriteLine("All = {0}", 	src.All(elem => elem % 3 == 0));
	    Console.WriteLine("Any = {0}", 	src.Any(elem => elem % 3 == 0));
	}

	
	//// 連結説明用
	static void LazySamples()
	{
	    var src = new[] {3, 2, 9, 6};

	    // 戻り値の型
	    Console.WriteLine("{0}", src.Select(elem => elem * 2));
	    
	    // 連結
	    var seq = src.Where(elem => elem % 2 == 1)
		.Select(elem => elem * 2);
	    foreach (var elem in seq)
	    {
		Console.Write("{0} ", elem);
	    }
	    Console.WriteLine();
	}
	
	static void Main(string[] args)
	{
	    MethodSamples();
	    Console.WriteLine("\n");
	    LazySamples();
	}
    }
}
