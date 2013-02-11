//
// IEnumerable ‚ÌƒTƒ“ƒvƒ‹
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
	    {
		_points = new Point[points.Length];
		for (int cnt = 0; cnt < points.Length ; cnt++)
		{
		    _points[cnt] = new Point(points[cnt]);
		}	
	    }	
	}

	/// <summary>
	/// —ñ‹“Žq‚ðŽæ“¾
	/// </summary>
	public IEnumerator GetEnumerator()
	{
	    return _points.GetEnumerator();
	}
    }
}
