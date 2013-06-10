//
// GZip �v���O���~���O�̃T���v��
//
// �R���p�C�� : csc GZipSample.cs

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
	///   gzip �̈��k
	/// </summary>
	/// <param name="srcpath"> ���̓t�@�C���̃p�X </param>
	/// <param name="gzpath"> �o�̓t�@�C��(gz)�̃p�X </param>
	public static void GZipCompress(string srcpath, string gzpath)
	{
	    // ���͗p�X�g���[��
	    using (FileStream fin = new FileStream(srcpath, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
		// �o�͗p�X�g���[��
		using (FileStream fout = File.Create(gzpath))
		{
		    // �o�͗p�X�g���[���� gzip �X�g���[���̃t�B���^�[��t����
		    using (GZipStream gzout = new GZipStream(fout, CompressionMode.Compress))
		    {
		    	fin.CopyTo(gzout);
		    }
		}
            }	    
	}

	/// <summary>
	///   gzip �̓W�J
	/// </summary>
	/// <param name="gzpath"> ���̓t�@�C��(gz)�̃p�X </param>
	/// <param name="destpath"> �o�̓t�@�C���̃p�X </param>
	public static void GZipDecompress(string gzpath, string destpath)
	{
	    // ���͗p�X�g���[��
	    using (FileStream fin = new FileStream(gzpath, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
		// �o�͗p�X�g���[��
		using (FileStream fout = File.Create(destpath))
		{
		    // ���͗p�X�g���[���� gzip �X�g���[���̃t�B���^�[��t����
		    using (GZipStream gzin = new GZipStream(fin, CompressionMode.Decompress))
		    {
			gzin.CopyTo(fout);
		    }
		}
            }	    
	}

	/// <summary>
	///   gzip �̎��s
	/// </summary>
	/// <param name="tpath"> �Ώۃt�@�C���̃p�X </param>
	/// 
	/// tpath �̖����� .gz �Ȃ�ΓW�J�A����ȊO�͈��k
	public static void GZipExecute(string tpath)
	{
	    Console.WriteLine("Input      : {0}", tpath);

	    string outfile;
	    var rx = new Regex(@"(.+)\.gz$");
	    var matchdat = rx.Match(tpath);
	    // ������ .gz ���ǂ����Ő؂�ւ�
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
	///   Usage �̕\��
	/// </summary>
	static void ShowUsage()
	{
	    // �v���O�������̎擾
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
	///   �v���O�����̃G���g���[�|�C���g
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
		// ��O�����͗�O�̃��b�Z�[�W��\�����Ă��邾��
		Console.WriteLine(ex.ToString());
		return 1;
	    }
	}
    }

}
