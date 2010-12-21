/*
 *
 * Copyright (C) 2010 by Masatoshi Itoh
 *     http://www.simpledungeon.com/
 *
 * This Program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This Program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with simple dungeon; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 * http://www.gnu.org/copyleft/gpl.html
 *
 */


using System;
using System.Collections;
using System.Collections.Generic;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Ink;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;

namespace SilverlightApplication1
{
    public class Mmoasp
    {
        String baseurl;
        public delegate void Action<T1, T2>(T1 obj1, T2 obj2);

        public Mmoasp(String baseurl)
        {
            this.baseurl = baseurl;
        }

        public void Call(String function, Dictionary<String, String> dict, Action<object, UploadStringCompletedEventArgs> callback)
        {
            Uri uri = new Uri(this.baseurl + function);
            String data = MakeParameters(dict);
            WebClient wc = new WebClient();
            wc.UploadStringCompleted += new UploadStringCompletedEventHandler(callback);
            wc.UploadStringAsync(uri, "POST",data);
            return;
        }

        public String MakeParameters(Dictionary<String, String> dict)
        {
            List<string> list = new List<string>();
            foreach (KeyValuePair<string, string> kvp in dict)
            {
                list.Add(kvp.Key + "=" + kvp.Value);
            }
            return String.Join("&", list);
        }
        
    }
    public class MmoChar
    {
        public string cid { get; set; }
        public string name { get; set; }
        public int map { get; set; }
        public int x { get; set; }
        public int y { get; set; }
        public UIElement img;
        public UIElement label;
    }
}
