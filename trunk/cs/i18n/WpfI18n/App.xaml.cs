using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using System.Globalization;	// 追加
using System.Threading;		// 追加

namespace WpfI18n
{
    /// <summary>
    /// App.xaml の相互作用ロジック
    /// </summary>
    public partial class App : Application
    {
        private void SetCurrentUICulture(string culname)
        {
            Thread.CurrentThread.CurrentUICulture = new CultureInfo(culname);
        }

        private void Application_Startup(object sender, StartupEventArgs e)
        {        
            // 環境変数を使用
            string langstr = System.Environment.GetEnvironmentVariable("TEMP_LANG");
            if (langstr != null)
            {
                SetCurrentUICulture(langstr);
            }

            // 引数を使用
            if (0 < e.Args.Length)
            {
                SetCurrentUICulture(e.Args[0]);
            }
        }
    }
}
