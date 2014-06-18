// コンパイル : csc FindFile.cs

using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;


namespace LinqSample
{
    class LazySample
    {
	public static IEnumerable<int> ReadValues(string fpath)
	{
	    var ptn = new Regex(@"\d+");
	    return File.ReadLines(fpath)
		.Select(line => ptn.Match(line).Value)
		.Where(str => !String.IsNullOrEmpty(str))
		.Select(str => int.Parse(str));
	}   
	
	public static void Main(string[] args)
	{
	    if (0 == args.Length)
	    {
		Console.WriteLine("LazySample.exe INFILE");
		return;
	    }
	    foreach (int val in ReadValues(args[0]))
	    {
		Console.WriteLine("{0}", val);
	    }
	}
    }
}
