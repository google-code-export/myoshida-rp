// コンパイル : csc BinFileIo.cs

using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;


namespace LinqSample
{
    class BinFileIo
    {
	// バイナリ形式でファイルに書き出し。
	private static void WriteData(string fpath, int[] data)
	{
	    using(BinaryWriter bw = new BinaryWriter(File.OpenWrite(fpath)))
    	    {
		// 先頭は要素数
		bw.Write(data.Length);
		foreach (int it in data)
		{
		    bw.Write(it);
		}
	    }
	}

	// 読み出し。
	private static IEnumerable<int> ReadData(string fpath)
	{
	    using(BinaryReader br = new BinaryReader(File.OpenRead(fpath)))
	    {
		// 先頭は要素数
		int siz = br.ReadInt32();
		for (int cnt = 0 ; cnt < siz ; cnt++)
		{
		    yield return br.ReadInt32();
		}
	    }
	}
	
	public static void Main()
	{
	    string testfile = "test.bin";
	    var ary = new[] {1, 2, 3, 4, 5};

	    // 作成
	    WriteData(testfile, ary);
	    
	    // 読み取り
	    foreach (int it in ReadData(testfile))
	    {
		Console.WriteLine("{0}", it);
	    }

	    // 作成したファイルを削除
	    FileInfo fi = new FileInfo(testfile);
	    fi.Delete();

	}
    }
}
