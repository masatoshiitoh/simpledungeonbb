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
using System.IO;
using System.Json;
using System.Linq;
using System.Net;
using System.Net.Browser;
using System.Windows;
using System.Windows.Browser;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;
using System.Threading;

namespace SilverlightApplication1
{
    public partial class MainPage : UserControl
    {
        Mmoasp service;
        public String message { get; set; }
        SynchronizationContext syncContext;
        public MainPage()
        {
            InitializeComponent();
            service = new Mmoasp("http://localhost:8002/service/hibari");
            syncContext = SynchronizationContext.Current;
        }
        public void OutputWrite()
        {
            txtMessage.Text = message;
        }

        private void btnLogin_Click(object sender, RoutedEventArgs e)
        {
            Dictionary<String, String> dict = new Dictionary<string,string>();
            dict.Add("id", txtId.Text);
            dict.Add("password", txtPass.Password);

            service.Call("/login", dict, callback);
        }

        void callback(object sender, UploadStringCompletedEventArgs e)
        {
            syncContext.Post(Receiver, e);
        }

        void Receiver(object arg)
        {
            UploadStringCompletedEventArgs e = arg as UploadStringCompletedEventArgs;
            String cid;
            String token;
            String result;

            JsonValue v = JsonObject.Parse(e.Result);

            JsonValue entry;

            switch (v.JsonType)
            {
                case JsonType.Array:
                    entry = v[0];
                    break;
                case JsonType.Object:
                    entry = v;
                    break;
                default:
                    return;
            }

            result = entry["result"];
            if (result == "ok")
            {
                cid = entry["cid"];
                token = entry["token"];

                Page1 p = new Page1();
                p.cid = cid;
                p.token = token;
                p.OutputWrite();
                this.Content = p;
            }
            else
            {
                this.message = entry["reason"];
                OutputWrite();
            }
        }

        private void mario_Loaded(object sender, RoutedEventArgs e)
        {

        }

        private void UserControl_Loaded(object sender, RoutedEventArgs e)
        {

        }

        private void button1_Click(object sender, RoutedEventArgs e)
        {
        }
    }
}
