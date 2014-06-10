// コンパイル : csc FindFile.cs

using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;


namespace LinqSample
{    
    class CatFile
    {
	public static void Cat(string fpath)
	{
	    foreach (string line in File.ReadLines(fpath))
	    {
		Console.WriteLine(line);
	    }
	}   
	
	public static void Main(string[] args)
	{
	    if (0 == args.Length)
	    {
		Console.WriteLine("CatFile.exe INFILE [..]");
		return;
	    }
	    foreach (string arg in args)
	    {
		Cat(arg);
	    }
	}
    }
}
