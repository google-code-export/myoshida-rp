// コンパイル : csc FindFile.cs

using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;


namespace LinqSample
{    
    class FindFile
    {
	public static IEnumerable<string> SearchDir(string dirPath, string str)
	{
	    DirectoryInfo di = new DirectoryInfo(dirPath);
	    IEnumerable<System.IO.FileInfo> fiList = di.GetFiles("*.*", SearchOption.AllDirectories);
	    return
		fiList
		.Where(fi => (0 <= fi.Name.IndexOf(str, StringComparison.CurrentCultureIgnoreCase)))
		.Select(fi => fi.FullName);
	}	

	public static void Main(string[] args)
	{
	    if (0 == args.Length)
	    {
		Console.WriteLine("FindFile.exe STRING [FOLDER]");
		return;
	    }
	    var str = args[0];
	    var dirPath = (1 < args.Length) ? args[1] : ".";

	    Console.WriteLine("Search \"{0}\" for \"{1}\"", dirPath, str);
	    foreach (string path in SearchDir(dirPath, str))
	    {
		Console.WriteLine(path);
	    }
	}
    }
}
