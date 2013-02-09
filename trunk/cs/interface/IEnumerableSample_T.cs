/// <summary>
///   IEnumerable<T> �̃T���v��
/// </summary>
///
/// �R���p�C�� : csc IEnumerableSample_T.cs

using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace IEnumerableSample_T
{
    class JaggedArray<TSource> : IEnumerable<TSource>
    {
	private List<TSource>[]  _list;
	
	public JaggedArray(int rowmax)
	{
	    _list = new List<TSource>[rowmax];
	}
	
	/// <summary>
	///   �v�f�̒ǉ�
	/// </summary>
	/// 1 ���ǉ�����Ă��Ȃ��̂�h�����߂ɍŏ��̈�����ʈ���
	/// 
	public bool Add(int row, TSource val, params TSource[] restvals)
	{
	    if (row < 0 || _list.Length <= row)
		return false;

	    // �܂��A�z�񂪂Ȃ���Ηp��
	    if (_list[row] == null)
	    {
		_list[row] = new List<TSource>();
	    }

	    // ������ǉ�
	    _list[row].Add(val);
	    foreach (TSource it in restvals)
	    {
		_list[row].Add(it);
	    }
	    
	    return true;
	}

	
	/// <summary>
	/// �񋓎q���擾
	/// </summary>
	public IEnumerator<TSource> GetEnumerator()
	{
	    foreach (List<TSource> sublist in _list)
	    {
		if (sublist != null)
		{
		    // �q���̔z����C�e���[�g
		    foreach (TSource val in sublist)
		    {
			yield return val;
		    }
		}	
	    }
	}

	IEnumerator IEnumerable.GetEnumerator()
	{
	    return this.GetEnumerator();
	}
    }

    
    class Program
    {
	// ���ʕ\���p�̊g�����\�b�h
	static string Dump<TSource>(IEnumerable<TSource> source)
	{
	    return "{" + string.Join(", ", source) + "}";
	}

	static void Main()
	{
	    JaggedArray<int> jagary = new JaggedArray<int>(5);
	    jagary.Add(0, 1);
	    jagary.Add(2, 1, 2, 3, 4);
	    jagary.Add(3, 1, 2);
	    jagary.Add(4, 5);

	    foreach (int it in jagary)
	    {
		Console.Write("{0} ", it);
	    }
	    Console.WriteLine("\n");

	    // LINQ
	    Console.WriteLine("Count = {0}", jagary.Count());
	    Console.WriteLine("3 Contains ? = {0}", jagary.Contains(3));
	    Console.WriteLine("Max = {0}", jagary.Max());
	    Console.WriteLine("Sum = {0}", jagary.Sum());
	    Console.WriteLine("Average = {0}", jagary.Average());
	    Console.WriteLine("ToArray = {0}", Dump(jagary.ToArray()));
	    Console.WriteLine("(Source) / 2.0 = {0}", Dump(jagary.Select(it=>it/2.0)));
	}
    }
}
