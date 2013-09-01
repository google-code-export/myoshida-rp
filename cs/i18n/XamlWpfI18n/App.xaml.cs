using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Windows;
using System.Threading;
using System.Globalization;

namespace XamlWpfI18n
{
    /// <summary>
    /// App.xaml の相互作用ロジック
    /// </summary>
    public partial class App : Application
    {
        private void SetCurrentUICulture(string culname)
        {
            CultureInfo ci = new CultureInfo(culname);
            Thread.CurrentThread.CurrentCulture = ci;
            Thread.CurrentThread.CurrentUICulture = ci;
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
