//
// IDisposable のサンプル
//
// コンパイル : csc IDisposableSample.cs

using System;
using System.Web;
using System.IO;
using System.Text;

namespace IDisposableSample
{
    class HtmlFileWriter : IDisposable
    {
	// 後処理の管理が必要なメンバー
	StreamWriter _writer;
	// 通常のメンバー
	string _title;
	// Dispose 
        private bool disposed = false;

	private void WriteHeader()
	{
	    _writer.WriteLine("<html>");
	    _writer.WriteLine("<head>");
	    _writer.WriteLine("<title> " + _title + " </title>");
	    _writer.WriteLine("</head>");
	    _writer.WriteLine("<body>");
	}	

	private void WriteFooter()
	{
	    _writer.WriteLine("</body>");
	    _writer.WriteLine("</html>");
	}	

	public HtmlFileWriter(string fpath, string title = "Sample")
	{
	    _title = title;
	    _writer = File.CreateText(fpath);
	    WriteHeader();
	}	


	// ファイルのクローズ
	public void Close()
	{
	    // ファイルストリームを真似て、
	    // Close() と Dispose() を同じ処理にします。
	    Dispose();
	}	

	
	public void Write(string str)
	{
	    _writer.Write(HttpUtility.HtmlEncode(str));
	}	

	public void WriteRaw(string str)
	{
	    _writer.Write(str);
	}	
	
	public void WriteLine(string str = "")
	{
	    Write(str);
	    _writer.WriteLine(" <br />");    
	}	
	
	public static HtmlFileWriter Create(string str)
	{
	    return new HtmlFileWriter(str);
	}


	// IDisposable に必須のメソッドの実装
        public void Dispose()
        {
            Dispose(true);
            // Dispose() によってリソースの解放を行ったので、
            // GC での解放が必要が無いことを GC に通知します。
            GC.SuppressFinalize(this);
        }

	// ファイナライザー(デストラクター)
        //
        // Dispose
        ~HtmlFileWriter()
        {
            Dispose(false);
        }

	
        // このメソッドの呼び出され方は 2 パターンあります。
        // 
        // disposing が true であれば、 Dispose() から呼び出されています。
	//
        // disposing が false であれば、 ファイナライザー(~HtmlFileWriter)
        // から呼び出されています。
        protected virtual void Dispose(bool disposing)
        {
            // Dispose がまだ実行されていないときだけ実行
            if(!this.disposed)
            {
                // disposing が true の場合(Dispose() が実行された場合)は
                // アンマネージリソースも解放します。
                if(disposing)
                {
                    // アンマネージリソースの解放
                    _title = null;
                }

		// マネージリソースの解放
		WriteFooter(); // 閉じる前にフッターを書く
		_writer.Dispose();

                disposed = true;
            }
        }

	
    } // Point

    class Program
    {
	static void Main(string[] args)
	{
	    string samplefpath = "sample.html";

	    using (HtmlFileWriter fp = HtmlFileWriter.Create(samplefpath))
	    {
		fp.WriteLine("test");
		fp.WriteLine("<test>");
		fp.WriteRaw("<strong>Test</strong>");
		fp.WriteLine();
	    }
	}
    }  
}


