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
	public static IEnumerable<string> ReadHeaderLines(string fpath, int linecnt)
	{
	    var ptn = new Regex(@"^\s*(#.*)?$");
	    return File.ReadLines(fpath)
		.Where(line => !ptn.IsMatch(line))
		.Take(linecnt);
	}   
	
	public static void Main(string[] args)
	{
	    if (0 == args.Length)
	    {
		Console.WriteLine("LazySample.exe INFILE");
		return;
	    }
	    foreach (string line in ReadHeaderLines(args[0], 3))
	    {
		Console.WriteLine(line);
	    }
	}
    }
}
