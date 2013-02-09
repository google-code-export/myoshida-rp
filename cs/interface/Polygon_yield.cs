//
// yield ���g���� IEnumerable �̃T���v��
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

	public int Count
	{
	    get { return _points.Length; }
	}

	/// <summary>
	/// �񋓎q���擾
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
