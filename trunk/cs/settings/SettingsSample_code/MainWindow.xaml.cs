using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace SettingsSample
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            // Settings の値をウィンドウに反映
            Left   = Properties.Settings.Default.MainWindow_Left;
            Top    = Properties.Settings.Default.MainWindow_Top;
            Width  = Properties.Settings.Default.MainWindow_Width;
            Height = Properties.Settings.Default.MainWindow_Height;
        }

        private void Window_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            // ウィンドウの値を Settings に格納
            Properties.Settings.Default.MainWindow_Left   = Left;
            Properties.Settings.Default.MainWindow_Top    = Top;
            Properties.Settings.Default.MainWindow_Width  = Width;
            Properties.Settings.Default.MainWindow_Height = Height;
            // ファイルに保存
            Properties.Settings.Default.Save();
        }
    }
}
