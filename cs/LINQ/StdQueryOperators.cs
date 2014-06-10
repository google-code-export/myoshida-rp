// コンパイル : csc StdQueryOperators.cs

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace LinqSample
{
    class StdQueryOperators
    {
	static string Dump<TSource>(IEnumerable<TSource> source)
	{
	    return "{" + string.Join(", ", source) + "}";
	}

	static void Main(string[] args)
	{
	    var src = new[] {3, 2, 9, 6};
	    Console.WriteLine("src           = {0}", Dump(src));

	    // Select (map)
	    var mapped = src.Select(elem => elem * 2); // {6, 4, 18, 12}
	    Console.WriteLine("map 'x 2'     = {0}", Dump(mapped));

	    // Where (filter)
	    var filtered = src.Where(elem => elem % 2 == 1); // {3, 9}
	    Console.WriteLine("fileter 'odd' = {0}", Dump(filtered));

	    var comp = src.Where(elem => elem % 2 == 1)
		.Select(elem => elem * 2);	
	    Console.WriteLine("fileter.map   = {0}", Dump(comp));

	    
	    // OrderBy (sort)
	    var possrc = new [] {
		    new { x = 1, y = 2 },
		    new { x = 3, y = 4 },
		    new { x = 1, y = 1 }
	    };
	    Console.WriteLine("src  = {0}", Dump(possrc));
	    var sorted = possrc.OrderBy(elem => elem.x).ThenBy(elem => elem.y);
	    Console.WriteLine("sort = {0}", Dump(sorted));	    
	}
    }
}
