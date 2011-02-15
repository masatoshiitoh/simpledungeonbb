using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace SilverlightApplication1
{
    public partial class Sprite : UserControl
    {
        public Sprite(String filename)
        {
            InitializeComponent();
            this.walking.Source = new BitmapImage(new Uri(filename, UriKind.RelativeOrAbsolute));
        }
    }
}
