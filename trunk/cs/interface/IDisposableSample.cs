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
	StreamWriter m_writer;

	private void WriteHeader()
	{
	    m_writer.WriteLine("<html>");
	    m_writer.WriteLine("<head>");
	    m_writer.WriteLine("<title> Sample </title>");
	    m_writer.WriteLine("</head>");
	    m_writer.WriteLine("<body>");
	}	

	private void WriteFooter()
	{
	    m_writer.WriteLine("</body>");
	    m_writer.WriteLine("</html>");
	}	

	
	#region 初期化

	public HtmlFileWriter(string fpath)
	{
	    m_writer = File.CreateText(fpath);
	    WriteHeader();
	}	

	#endregion

	public void Close()
	{
	    if (m_writer != null)
	    {
		WriteFooter();
		m_writer.Close();
		m_writer = null;
	    }	
	}	

	public void Dispose()
	{
	    Close();
	}
	
	public void Write(string str)
	{
	    m_writer.Write(HttpUtility.HtmlEncode(str));
	}	

	public void WriteRaw(string str)
	{
	    m_writer.Write(str);
	}	
	
	public void WriteLine(string str = "")
	{
	    Write(str);
	    m_writer.WriteLine(" <br />");    
	}	
	
	public static HtmlFileWriter Create(string str)
	{
	    return new HtmlFileWriter(str);
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


