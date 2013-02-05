//
// yield を使った IEnumerable のサンプル
//

using System;
using System.Collections;


namespace Geometry
{

    class Polygon : IEnumerable
    {
	private Point[] _points;

	public Polygon(Point[] points)
	{
	    if (points != null)
		_points = (Point[])points.Clone();
	}

	public int Count
	{
	    get { return _points.Length; }
	}

	/// <summary>
	/// 列挙子を取得
	/// </summary>
	public IEnumerator GetEnumerator()
	{
	    foreach (Point pos in _points)
	    {
		yield return pos;
	    }
	}
    }
}
