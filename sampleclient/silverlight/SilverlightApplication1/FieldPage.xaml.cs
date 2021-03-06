﻿/*
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

        Boolean listenable = true; // this page is be instanciated after authentication.

        Dictionary<String, MmoChar> mmoChars = new Dictionary<String, MmoChar>();
        List<String> chatLog = new List<string>();

        public Page1()
        {
            InitializeComponent();

            service = new Mmoasp("http://simpledungeons.com:8002/service/hibari");
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
        private MmoChar LookupByPos(int x, int y)
        {
            foreach (MmoChar c in mmoChars.Values)
            {
                if (c.x == x && c.y == y)
                {
                    return c;
                }
            }
            return null;
        }
        public void OutputWrite()
        {
            txtInput.Text = cid;
        }
        public void OnTimer()
        {
            framecount++;
            if (framecount % 30 == 0)
            {
                // 1 sec.=60frames ( set in App.xaml.cs )
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
            if (listenable == false) return;
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
            if (listenable == false) return;

            UploadStringCompletedEventArgs e = arg as UploadStringCompletedEventArgs;
            txtReceive.Text = e.Result;

            
            foreach (MmoChar c in mmoChars.Values)
            {
                try
                {
                    Field.Children.Remove(c.img);
                    Field.Children.Remove(c.label);
                }
                catch (Exception ee)
                {
                    // do nothing.
                }
            }
            
            JsonValue v = JsonObject.Parse(e.Result); // Exception here! list_to_know call after logout cause it. fix!!
            foreach (JsonValue jv in v)
            {
                try
                {
                    if (jv["type"] == "pc")
                    {

                        MmoChar c = null;
                        mmoChars.TryGetValue(jv["cid"], out c);
                        if (c != null)
                        {
                            // update information.

                            c.x = (jv.ContainsKey("x") ? jv["x"] : new JsonPrimitive(1));
                            c.y = (jv.ContainsKey("y") ? jv["y"] : new JsonPrimitive(1));

                            Canvas.SetLeft(c.img, c.x * 32);
                            Canvas.SetTop(c.img, c.y * 32);

                            Canvas.SetLeft(c.label, c.x * 32);
                            Canvas.SetTop(c.label, c.y * 32 - 24);

                            Field.Children.Add(c.img);
                            Field.Children.Add(c.label);

                        }
                        else
                        {
                            // add new character.

                            c = new MmoChar();
                            c.cid = jv["cid"];
                            c.name = jv["name"];
                            c.x = (jv.ContainsKey("x") ? jv["x"] : new JsonPrimitive(1));
                            c.y = (jv.ContainsKey("y") ? jv["y"] : new JsonPrimitive(1));

                            Sprite ca = new Sprite("char.png");
                            ca.Height = 32;
                            ca.Width = 32;

                            c.img = ca;
                            Canvas.SetLeft(c.img, c.x * 32);
                            Canvas.SetTop(c.img, c.y * 32);

                            Label la = new Label();
                            la.Height = 32;
                            la.Width = 200;
                            la.Content = c.name;
                            la.FontFamily = new System.Windows.Media.FontFamily("Comic Sans MS, Verdana");
                            la.Foreground = new SolidColorBrush(Colors.Black);
                            c.label = la;
                            Canvas.SetLeft(c.label, c.x * 32);
                            Canvas.SetTop(c.label, c.y * 32 - 24);

                            // fill attribute update in here.
                            mmoChars.Add(c.cid, c);
                            Field.Children.Add(c.img);
                            Field.Children.Add(c.label);

                        }
                    }
                    //txtLog.Text += jv["type"] + Environment.NewLine;
                    else if (jv["type"] == "npc")
                    {

                        MmoChar c = null;
                        mmoChars.TryGetValue(jv["cid"], out c);
                        if (c != null)
                        {
                            // update information.

                            c.x = (jv.ContainsKey("x") ? jv["x"] : new JsonPrimitive(1));
                            c.y = (jv.ContainsKey("y") ? jv["y"] : new JsonPrimitive(1));

                            Canvas.SetLeft(c.img, c.x * 32);
                            Canvas.SetTop(c.img, c.y * 32);

                            Canvas.SetLeft(c.label, c.x * 32);
                            Canvas.SetTop(c.label, c.y * 32 - 24);

                            Field.Children.Add(c.img);
                            Field.Children.Add(c.label);
                        }
                        else
                        {
                            // add new character.

                            c = new MmoChar();
                            c.cid = jv["cid"];
                            c.name = jv["name"];
                            c.x = (jv.ContainsKey("x") ? jv["x"] : new JsonPrimitive(1));
                            c.y = (jv.ContainsKey("y") ? jv["y"] : new JsonPrimitive(1));

                            Sprite ca = new Sprite("slime.png");
                            ca.Height = 32;
                            ca.Width = 32;

                            c.img = ca;
                            Canvas.SetLeft(c.img, c.x * 32);
                            Canvas.SetTop(c.img, c.y * 32);

                            Label la = new Label();
                            la.Height = 32;
                            la.Width = 200;
                            la.Content = c.name;
                            la.FontFamily = new System.Windows.Media.FontFamily("Comic Sans MS, Verdana");
                            la.Foreground = new SolidColorBrush(Colors.Black);
                            c.label = la;
                            Canvas.SetLeft(c.label, c.x * 32);
                            Canvas.SetTop(c.label, c.y * 32 - 24);


                            // fill attribute update in here.
                            mmoChars.Add(c.cid, c);
                            Field.Children.Add(c.img);
                            Field.Children.Add(c.label);

                        }
                    }

                    else if (jv["type"] == "attack")
                    {
                        MmoChar attacked = null;
                        mmoChars.TryGetValue(jv["cid"], out attacked);  // find character from known list
                        Int32 i = jv["damage"];
                        
                        if (attacked != null)
                        {
                            txtLog.Text = (attacked.name + " got damage : " + i + Environment.NewLine) + txtLog.Text;
                        }
                        else
                        {
                            txtLog.Text = (jv["cid"] + " got damage : " + i + Environment.NewLine) + txtLog.Text;
                        }
                    }

                    else if (jv["type"] == "killed")
                    {
                        MmoChar attacked = null;
                        mmoChars.TryGetValue(jv["cid"], out attacked);  // find character from known list

                        if (attacked != null)
                        {
                            txtLog.Text = (attacked.name + " was killed." + Environment.NewLine) + txtLog.Text;
                        }
                        else
                        {
                            txtLog.Text = (jv["cid"] + " was killed." + Environment.NewLine) + txtLog.Text;
                        }
                    }

                    else if (jv["type"] == "talk")
                    {
                        MmoChar talker = null;
                        mmoChars.TryGetValue(jv["cid"], out talker);
                        if (talker != null)
                        {
                            txtLog.Text = (talker.name + " : " + jv["content"] + Environment.NewLine) + txtLog.Text;
                            Random r = new Random();
                            Canvas.SetLeft(talker.img, r.Next(320));
                            Canvas.SetTop(talker.img, r.Next(240));
                        }
                        else
                        {
                            txtLog.Text = (jv["cid"] + " : " + txtInput.Text + Environment.NewLine) + txtLog.Text;
                        }
                    }

                    else if (jv["type"] == "move")
                    {
                        MmoChar walker = null;
                        mmoChars.TryGetValue(jv["cid"], out walker);
                        if (walker != null)
                        {
                            txtLog.Text = String.Format("MOVE: {0} move from ({1}, {2}) to ({3},{4}) within {5}ms.", new object[] { walker.name, jv["from_x"], jv["from_y"], jv["to_x"], jv["to_y"], jv["duration"] }) + Environment.NewLine + txtLog.Text;
                            walker.x = jv["from_x"];
                            walker.y = jv["from_y"];
                            Canvas.SetLeft(walker.img, walker.x * 32);
                            Canvas.SetTop(walker.img, walker.y * 32);
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
                            txtLog.Text = (exiter.name + " is logged out." + Environment.NewLine) + txtLog.Text;
                            Field.Children.Remove(exiter.img);
                            Field.Children.Remove(exiter.label);
                            mmoChars.Remove(jv["cid"]);
                        }
                    }
                    else if (jv["type"] == "remove")
                    {
                        MmoChar exiter = null;
                        mmoChars.TryGetValue(jv["cid"], out exiter);
                        if (exiter != null)
                        {
                            Field.Children.Remove(exiter.img);
                            Field.Children.Remove(exiter.label);
                            mmoChars.Remove(jv["cid"]);
                        }
                    }
                }
                catch (Exception ex)
                {
                    //txtLog.Text = ex.Message;
                }
            }

        }

        private void btnLogout_Click(object sender, RoutedEventArgs e)
        {
            Dictionary<String, String> dict = new Dictionary<string, string>();
            dict.Add("token", token);

            listenable = false;
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
                /*
                MmoChar talker = null;
                mmoChars.TryGetValue(cid, out talker);
                if (talker != null)
                {
                    txtLog.Text = (talker.name + " : " + txtInput.Text + Environment.NewLine) + txtLog.Text;
                }
                else
                {
                    txtLog.Text = (cid + " : " + txtInput.Text + Environment.NewLine) + txtLog.Text;
                }
                 * */
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

        private void txtInput_TextChanged(object sender, TextChangedEventArgs e)
        {

        }

        private void mapLeftDown(object sender, MouseButtonEventArgs e)
        {
            Point p = e.GetPosition(Field);
            int x = ((int)p.X) / 32;
            int y = ((int)p.Y) / 32;
            MmoChar mc = LookupByPos(x, y);
            if (mc == null)
            {
                txtLog.Text = ("Clicked " + x.ToString() + " " + y.ToString() + Environment.NewLine) + txtLog.Text;


                Dictionary<String, String> dict = new Dictionary<string, string>();
                dict.Add("token", token);
                dict.Add("x", x.ToString());
                dict.Add("y", y.ToString());

                service.Call("/move/" + cid, dict, move_callback);
            }
            else
            {
                txtLog.Text = ("Clicked " + mc.name + Environment.NewLine) + txtLog.Text;
                
                Dictionary<String, String> dict = new Dictionary<string, string>();
                dict.Add("token", token);

                service.Call("/attack/" + cid + "/" + mc.cid, dict, attack_callback);
            }
        }

        void move_callback(object sender, UploadStringCompletedEventArgs e)
        {
            syncContext.Post(move_receiver, e);
        }

        void move_receiver(object arg)
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

        void attack_callback(object sender, UploadStringCompletedEventArgs e)
        {
            syncContext.Post(attack_receiver, e);
        }

        void attack_receiver(object arg)
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
