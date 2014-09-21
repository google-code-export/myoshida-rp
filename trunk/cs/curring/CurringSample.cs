// コンパイル : csc CurringSample.cs

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CurriedSample
{
    class Program
    {
	static int Plus(int a, int b)
	{
	    return a + b;
	}

	static Func<int, int> CurriedPlus(int a)
	{
	    return (b) => a + b;
	}

	static Func<int, int> CurriedPlus2(int a)
	{
	    return (b) => Plus(a, b);
	}
	
	

	/// <summary>
	///   シーケンスのダンプメソッド
	/// </summary>
	static string Dump<TSource>(IEnumerable<TSource> source)
	{
	    return "{" + string.Join(", ", source) + "}";
	}

	static void Main(string[] args)
	{
	    Console.WriteLine("{0}", Plus(2, 5));           // 7
	    Console.WriteLine("{0}", CurriedPlus(2)(5));    // 7
	    Console.WriteLine("{0}", CurriedPlus2(2)(5));   // 7

	    Console.WriteLine();	    
	    
	    var src = new[] {1, 2, 3, 4};
	    Console.WriteLine("{0}", Dump(src.Select(p => 2 + p )));     // {3, 4, 5, 6}
	    Console.WriteLine("{0}", Dump(src.Select(CurriedPlus(2))));  // {3, 4, 5, 6}
	    Console.WriteLine("{0}", Dump(src.Select(CurriedPlus2(2))));  // {3, 4, 5, 6}
	}
    }
}
