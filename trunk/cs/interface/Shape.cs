/// <summary>
///   yield ���g���� IEnumerable �̃T���v��
/// </summary>
/// 
/// �R���p�C�� : csc Shape.cs Point.cs 

using System;
using System.Collections;


namespace Geometry
{

    /// <summary>
    ///   �}�`�̒��ۃN���X
    /// </summary>
    abstract class Shape : IEnumerable
    {
	public Shape()
	{
	}

	/// <summary>
	///   �񋓎q���擾
	/// </summary>
	abstract public IEnumerator GetEnumerator();
    }

    /// <summary>
    ///   �����`�N���X
    /// </summary>
    class Rectangle : Shape
    {
	public int x  { get; set; }
	public int y  { get; set; }
	public int w  { get; set; }
	public int h  { get; set; }

	public Rectangle(int x, int y, int w, int h)
	{
	    this.x = x;
	    this.y = y;
	    this.w = w;
	    this.h = h;
	}

	/// <summary>
	///   �񋓎q���擾
	/// </summary>
	override public IEnumerator GetEnumerator()
	{
	    yield return new Point(x,   y);
	    yield return new Point(x+w, y);
	    yield return new Point(x+w, y+w);
	    yield return new Point(x,   y+w);
	}
    }

    
    /// <summary>
    ///   ���p�`�N���X
    /// </summary>
    class Polygon : Shape
    {
	private Point[] _points;

	public Polygon(Point[] points)
	{
	    if (points != null)
		_points = (Point[])points.Clone();
	}

	/// <summary>
	/// �񋓎q���擾
	/// </summary>
	override public IEnumerator GetEnumerator()
	{
	    foreach (Point pos in _points)
	    {
		yield return pos;
	    }
	}
    }

    class Program
    {
	static void DumpShape(Shape fig)
	{
	    Console.Write("{0} = [ ", fig);
	    foreach (Point pos in fig)
	    {
		Console.Write("{0} ", pos);
	    }
	    Console.WriteLine("]");	    
	}	
	
	static void Main()
	{
	    Shape[] shapes = new Shape[] {
		new Rectangle(10, 5, 10, 5),
		new Polygon(new Point[] {
			new Point(0, 0),
			new Point(5, 0),
			new Point(0, 5)
		    })
	    };
	    
	    foreach (Shape fig in shapes)
	    {
		DumpShape(fig);
	    }
	}
    }

}
