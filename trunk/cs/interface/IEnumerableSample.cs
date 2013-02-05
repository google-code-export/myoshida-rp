//
// IEnumerable �̃T���v��
//
// �R���p�C�� (�ʏ��)   : csc IEnumerableSample.cs Point.cs Polygon_Enumerator.cs
// �R���p�C�� (yield ��) : csc IEnumerableSample.cs Point.cs Polygon_yield.cs

using System;
using System.Collections;

using Geometry;


namespace IEnumerableSample
{

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
	}
    }

}
