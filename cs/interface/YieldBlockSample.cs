//
// yield �u���b�N�̃T���v��
//
// �R���p�C�� : csc YieldBlockSample.cs Point.cs

using System;
using System.Collections;

using Geometry;


namespace IEnumerableSample
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
	/// �񋓎q���擾
	/// </summary>
	public IEnumerator GetEnumerator()
	{
	    foreach (Point pos in _points)
	    {
		yield return pos;
	    }
	}

	/// <summary>
	///   �����܂���̎擾
	/// </summary>
	public IEnumerable ClosedPolyline()
	{
	    foreach (Point pos in _points)
	    {
		yield return pos;
	    }
	    yield return _points[0];
	}
	
    }

    class Program
    {
	static void Main()
	{
	    Point[] points = new Point[] {
		new Point(0, 0),
		new Point(5, 0),
		new Point(0, 5)
	    };
	    Polygon poly = new Polygon(points);
	    
	    Console.Write("Polygon = {\n  ");
	    foreach (Point pos in poly)
	    {
		Console.Write("{0} ", pos);
	    }
	    Console.WriteLine("\n}");

	    
	    Console.Write("Closed Polyline = {\n  ");
	    foreach (Point pos in poly.ClosedPolyline())
	    {
		Console.Write("{0} ", pos);
	    }
	    Console.WriteLine("\n}");
	}
    }

}
