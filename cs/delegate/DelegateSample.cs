//
// event のサンプル
//
// コンパイル : csc SimpleEventSample.cs

using System;


namespace DelegateSample
{
    // Delegate の型を宣言
    delegate void HelloDelegate(string name);
    
    class Foo
    {
	// メソッド
    	public void Hello(string name)
    	{
    	    Console.WriteLine("(method) : Hello {0}!", name);
    	}
    }
    
    class Program
    {
	// static な関数
    	static void StaticHello(string name)
    	{
    	    Console.WriteLine("(static) : Hello {0}!", name);
    	}

    	static void Main()
	{
	    Foo foo = new Foo();

	    // delegate の変数
	    HelloDelegate funcs = delegete(){};
    	    
    	    funcs = foo.Hello;		// メソッドの格納
    	    funcs += StaticHello;	// static 関数の格納
	    // 無名関数の格納
    	    funcs += (string name) => {
		Console.WriteLine("(lambda) : Hello {0}!", name);
	    };
	    // 無名関数の格納 (delegete キーワード版)
    	    funcs += delegate(string name) {
		Console.WriteLine("(delegate) : Hello {0}!", name);
	    };

    	    // 格納した関数の呼び出し
    	    funcs("world");
	}
    }    
}


