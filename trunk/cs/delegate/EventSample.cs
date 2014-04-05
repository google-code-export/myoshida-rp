//
// event のサンプル
//
// コンパイル : csc SimpleEventSample.cs

using System;


namespace EventSample
{
    delegate void HelloDelegate(string name);
    
    class Button
    {
	// イベント
	// (クラス定義内でしか event は定義できない)
	public event HelloDelegate SomeEvent;

	// イベントを発生させる
	public void Clicked(string name)
	{
	    // イベントの発生
	    // (直接には Foo クラス内でしか呼べない)
	    SomeEvent(name);
	}
    }

    class Foo
    {
    	public void Hello(string name)
    	{
    	    Console.WriteLine("(method) : Hello {0}!", name);
    	}
    }

    class Program
    {
    	static void StaticHello(string name)
    	{
    	    Console.WriteLine("(static) : Hello {0}!", name);
    	}

	static void Main(string[] args)
	{
	    Foo foo = new Foo();

	    Button btn = new Button();

	    // 関数の登録(アタッチ)
	    btn.SomeEvent += foo.Hello;
	    btn.SomeEvent += StaticHello;
	    btn.SomeEvent += (string name) => {
		Console.WriteLine("(lambda) : Hello {0}!", name);
	    };

	    // イベントの発生
	    btn.Clicked("world");
	}
    }
}


