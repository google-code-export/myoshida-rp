// コンパイル : csc FindFile.cs

using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;


namespace LinqSample
{    
    class FindFile
    {
	/// <summary>
	///   dirPath 以下のファイルの中で str の文字列を含むファイル名のファイル群を返す
	/// </summary>
	public static IEnumerable<string> SearchDir(string dirPath, string str)
	{
	    // 指定フォルダーのファイルをサブフォルダーまで列挙
	    DirectoryInfo di = new DirectoryInfo(dirPath);
	    IEnumerable<System.IO.FileInfo> fiList = di.GetFiles("*.*", SearchOption.AllDirectories);

	    return
		fiList
		.Where(fi =>	// str を含むかでフィルター
		       (0 <= fi.Name.IndexOf(str, StringComparison.CurrentCultureIgnoreCase)))
		.Select(fi => fi.FullName); // フルパスに変換
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

	    // 使用
	    foreach (string path in SearchDir(dirPath, str))
	    {
		Console.WriteLine(path);
	    }
	}
    }
}
