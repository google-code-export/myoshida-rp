//
// GZip プログラミングのサンプル
//
// コンパイル : csc GZipSample.cs

using System;
using System.IO;
using System.IO.Compression;
using System.Reflection;
using System.Text.RegularExpressions;

namespace GZipSample
{
    
    class Program
    {
	/// <summary>
	///   gzip の圧縮
	/// </summary>
	/// <param name="srcpath"> 入力ファイルのパス </param>
	/// <param name="gzpath"> 出力ファイル(gz)のパス </param>
	public static void GZipCompress(string srcpath, string gzpath)
	{
	    // 入力用ストリーム
	    using (FileStream fin = new FileStream(srcpath, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
		// 出力用ストリーム
		using (FileStream fout = File.Create(gzpath))
		{
		    // 出力用ストリームに gzip ストリームのフィルターを付ける
		    using (GZipStream gzout = new GZipStream(fout, CompressionMode.Compress))
		    {
		    	fin.CopyTo(gzout);
		    }
		}
            }	    
	}

	/// <summary>
	///   gzip の展開
	/// </summary>
	/// <param name="gzpath"> 入力ファイル(gz)のパス </param>
	/// <param name="destpath"> 出力ファイルのパス </param>
	public static void GZipDecompress(string gzpath, string destpath)
	{
	    // 入力用ストリーム
	    using (FileStream fin = new FileStream(gzpath, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
		// 出力用ストリーム
		using (FileStream fout = File.Create(destpath))
		{
		    // 入力用ストリームに gzip ストリームのフィルターを付ける
		    using (GZipStream gzin = new GZipStream(fin, CompressionMode.Decompress))
		    {
			gzin.CopyTo(fout);
		    }
		}
            }	    
	}

	/// <summary>
	///   gzip の実行
	/// </summary>
	/// <param name="tpath"> 対象ファイルのパス </param>
	/// 
	/// tpath の末尾が .gz ならば展開、それ以外は圧縮
	public static void GZipExecute(string tpath)
	{
	    Console.WriteLine("Input      : {0}", tpath);

	    string outfile;
	    var rx = new Regex(@"(.+)\.gz$");
	    var matchdat = rx.Match(tpath);
	    // 末尾が .gz かどうかで切り替え
	    if (matchdat.Success)
	    {
		Console.Write("Decompress : ");
		outfile = matchdat.Groups[1].Value;
		GZipDecompress(tpath, outfile);
	    }
	    else
	    {
		Console.Write("Compress   : ");
		outfile = tpath + ".gz";
		GZipCompress(tpath, outfile);
	    }
	    Console.WriteLine(outfile);		
	}

	/// <summary>
	///   Usage の表示
	/// </summary>
	static void ShowUsage()
	{
	    // プログラム名の取得
	    Assembly myAssembly = Assembly.GetEntryAssembly();
	    string path = Path.GetFileName(myAssembly.Location);

	    Console.WriteLine("Usage:");
	    Console.WriteLine("    {0} FILE ", path);
	    Console.WriteLine();
	    Console.WriteLine("FILE", path);
	    Console.WriteLine("    *.gz     : Decompress");
	    Console.WriteLine("    not *.gz : Compress.");
	}

	/// <summary>
	///   プログラムのエントリーポイント
	/// </summary>
	static int Main(string[] args)
	{
	    if (args.Length == 0)
	    {
		ShowUsage();
		return 1;
	    }
	    
	    try
	    {
		GZipExecute(args[0]);
		return 0;
	    }	
	    catch (System.Exception ex)
	    {
		// 例外処理は例外のメッセージを表示しているだけ
		Console.WriteLine(ex.ToString());
		return 1;
	    }
	}
    }

}
