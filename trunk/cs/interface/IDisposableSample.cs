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
	// Dispose したかどうか
        private bool _disposed = false;

	
	// コンストラクター
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
	    CheckDisposed();
	    _writer.Write(HttpUtility.HtmlEncode(str));
	}	

	public void WriteRaw(string str)
	{
	    CheckDisposed();
	    _writer.Write(str);
	}	
	
	public void WriteLine(string str = "")
	{
	    Write(str);
	    _writer.WriteLine(" <br />");    
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
	// Dispose() が呼び出されていない場合のみ
	// 実行されます。
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
            if(!_disposed)
            {
                // disposing が true の場合(Dispose() が実行された場合)は
                // マネージリソースも解放します。
                if(disposing)
                {
		    // マネージリソースの解放

		    // マネージリソースは null を指定
		    _title = null;
                }
		
		// アンマネージリソースの解放

		// 閉じる前にフッターを書く
		WriteFooter();
		_writer.Dispose();

                _disposed = true;
            }
        }

	private void CheckDisposed()
	{
	    if (_disposed)
		throw new ObjectDisposedException(GetType().FullName);		
	}

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
	
    } // HtmlFileWriter

    class Program
    {
	static void Main(string[] args)
	{
	    string samplefpath = "sample.html";

	    using (HtmlFileWriter fp = new HtmlFileWriter(samplefpath))
	    {
		fp.WriteLine("test");
		fp.WriteLine("<test>");
		fp.WriteRaw("<strong>Test</strong>");
		fp.WriteLine();
	    }
	}
    }  
}


