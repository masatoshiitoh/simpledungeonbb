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
using System.Collections.Generic;
using System.Json;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Browser;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;
using System.Windows.Navigation;
using System.Threading;

namespace SilverlightApplication1
{

    public partial class Page1 : Page
    {
        public String cid { get; set; }
        public String token { get; set; }
        public String result { get; set; }
        Mmoasp service;
        SynchronizationContext syncContext;
        int framecount;

        Dictionary<String, MmoChar> mmoChars = new Dictionary<String, MmoChar>();
        List<String> chatLog = new List<string>();

        public Page1()
        {
            InitializeComponent();
            service = new Mmoasp("http://219.121.56.65:8002/service/hibari");
            syncContext = SynchronizationContext.Current;
            CompositionTarget.Rendering += new EventHandler(CompositionTarget_Rendering);
            framecount = 0;

        }
        void CompositionTarget_Rendering(object sender, EventArgs e)
        {
            OnTimer();
        }
        // run this function when user has been moved here.
        protected override void OnNavigatedTo(NavigationEventArgs e)
        {


        }
        public void OutputWrite()
        {
            txtInput.Text = cid;
        }
        public void OnTimer()
        {
            framecount++;
            if (framecount % 120 == 0)
            {
                // 2 sec.
                // txtLog.Text = FormatDateTime(DateTime.Now);
                framecount = 0;
                get_list_to_know();
            }
        }
        internal string FormatDateTime(DateTime dt)
        {
            string s = dt.ToString("Now,  M/d/yyyy tt h m s");
            return s;
        }

        void get_list_to_know()
        {
            Dictionary<String, String> dict = new Dictionary<string, string>();
            dict.Add("token", token);
            service.Call("/listtoknow/" + cid, dict, list_to_know_callback);
        }

        void list_to_know_callback(object sender, UploadStringCompletedEventArgs e)
        {
            syncContext.Post(list_to_know_receiver, e);
        }

        void list_to_know_receiver(object arg)
        {
            UploadStringCompletedEventArgs e = arg as UploadStringCompletedEventArgs;
            try
            {
                txtLog.Text = FormatDateTime(DateTime.Now) + Environment.NewLine;
                JsonValue v = JsonObject.Parse(e.Result);
                foreach (JsonValue jv in v)
                {
                    //txtLog.Text += jv["type"] + Environment.NewLine;
                    if (jv["type"] == "talk")
                    {
                        MmoChar talker = null;
                        mmoChars.TryGetValue(jv["cid"], out talker);
                        if (talker != null)
                        {
                            txtLog.Text += (talker.name + " : " + jv["content"] + Environment.NewLine);
                        }
                    }
                    else if (jv["type"] == "pc")
                    {
                        MmoChar c = new MmoChar();
                        c.cid = jv["cid"];
                        c.name = jv["name"];
                        // fill attribute update in here.
                        mmoChars.Add(c.cid, c);
                    }
                    else if (jv["type"] == "move")
                    {
                        // jv["cid"]
                        MmoChar walker = null;
                        mmoChars.TryGetValue(jv["cid"], out walker);
                        if (walker != null)
                        {
                            txtLog.Text += String.Format("MOVE: {0} move from ({1}, {2}) to ({3},{4}) within {5}ms.", new object[] { walker.name, jv["from_x"], jv["from_y"], jv["to_x"], jv["to_y"], jv["duration"] }) + Environment.NewLine;
                        }
                    }
                    else if (jv["type"] == "login")
                    {
                    }
                    else if (jv["type"] == "logout")
                    {
                        MmoChar exiter = null;
                        mmoChars.TryGetValue(jv["cid"], out exiter);
                        if (exiter != null)
                        {
                            txtLog.Text += (exiter.name + " is logged out." + Environment.NewLine);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                // do nothing
            }
        }

        private void btnLogout_Click(object sender, RoutedEventArgs e)
        {
            Dictionary<String, String> dict = new Dictionary<string, string>();
            dict.Add("token", token);

            service.Call("/logout/" + cid, dict, logout_callback);
        }

        void logout_callback(object sender, UploadStringCompletedEventArgs e)
        {
            syncContext.Post(logout_receiver, e);
        }

        void logout_receiver(object arg)
        {
            JsonValue entry;
            UploadStringCompletedEventArgs e = arg as UploadStringCompletedEventArgs;

            JsonValue v = JsonObject.Parse(e.Result);

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

            if (entry["result"] == "ok")
            {
                CompositionTarget.Rendering -= new EventHandler(CompositionTarget_Rendering);

                MainPage p = new MainPage();
                this.Content = p;
            }
        }

        private void txtInput_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Enter)
            {
                sendChat(txtInput.Text, "open");
                txtInput.Text = "";
            }
        }

        private void sendChat(String content, String mode)
        {
            Dictionary<String, String> dict = new Dictionary<string, string>();
            dict.Add("token", token);
            dict.Add("talked", content);

            service.Call("/talk/" + cid, dict, talk_callback);
        }
        void talk_callback(object sender, UploadStringCompletedEventArgs e)
        {
            syncContext.Post(talk_receiver, e);
        }

        void talk_receiver(object arg)
        {
            JsonValue entry;
            UploadStringCompletedEventArgs e = arg as UploadStringCompletedEventArgs;

            JsonValue v = JsonObject.Parse(e.Result);

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

            // just only return.
            if (entry["result"] == "ok")
            {
                return;
            }
        }

    }
}
