/// <summary>
///   点クラス (IComparable のサンプル用)
/// </summary>

using System;

namespace Geometry
{
    class Point : IComparable
    {
	/// <summary>
	///   x 座標
	/// </summary>
	public int x  { get; set; }

	/// <summary>
	///   y 座標
	/// </summary>
	public int y  { get; set; }


	/// <summary>
	/// 座標値 (x, y) を与えて初期化。
	/// </summary>
	/// <param name="x">x 座標値</param>
	/// <param name="y">y 座標値</param>
	public Point(int x, int y)
	{
	    this.x = x;
	    this.y = y;
	}


	/// <summary>
	///   比較メソッド
	/// </summary>
	public int CompareTo(Object obj)
	{
	    if (obj == null) return 1;
	     
	    Point other = (Point)obj;
	    if (other.x == x) {
		return y - other.y;
	    }	
	    return x - other.x;
	}


	/// <summary>
	///   文字列化メソッド
	/// </summary>
	public override string ToString()
	{
	    return "(" + x + ", " + y + ")";
	}
	
    } // Point
  
}
