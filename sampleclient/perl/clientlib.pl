#!/usr/bin/perl
#
#      Copyright (C) 2010 by Masatoshi Itoh
#      http://www.simpledungeon.com/
#
#  This Program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2, or (at your option)
#  any later version.
#
#  This Program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with simple dungeon; see the file COPYING.  If not, write to
#  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#  http://www.gnu.org/copyleft/gpl.html
#



use LWP::UserAgent;
use HTTP::Request::Common qw(POST GET);
use Data::Dumper;
use JSON;

# This is a 'simple dungeon' api caller library.

my $base_url = "http://localhost:8002";

my $sv = "hibari";
my $cid;
my $token;


sub autotest {
	($uid, $pw) = @_;
	my @cmd_begin = (
		"subscribe $uid $pw",
		"passwd $uid $pw newpw",
		"login $uid newpw",
	);

	my @cmd_loop = (
		"move 3 3",
		"talk hoo",
		"get",
		"move 1 3",
		"talk foo!",
		"get",
		"move 2 1",
		"talk goba-",
	);

	my @cmd_end = (
		"talk goodbye!",
		"logout",
	);



	foreach my $line  (@cmd_begin) {
		&line_handler($line);
		sleep(1);
	}

	for (my $i = 0; $i < 10; $i ++) {
		foreach my $line  (@cmd_loop) {
			&line_handler($line);
			sleep(1);
		}
	}

	foreach my $line  (@cmd_end) {
		&line_handler($line);
		sleep(1);
	}

}

sub interactive {
	print "prompt:";
	while ($line = <STDIN>) {
		chomp($line);
		&line_handler($line);
		print "cid is $cid, token is $token\n";
		print "prompt:";
	}
	exit(0);
}

sub line_handler
{
	my $line = shift;
	chomp($line);
	($cmd, $args) = split(/ /, $line, 2);
	print $cmd . "\n";
	print $args . "\n";

	if ($cmd eq 'subscribe') {
		$o = &subscribe($args);
		$cid = $o->{cid};
		$token = $o->{token};
	}elsif ($cmd eq 'passwd') {
		$o = &passwd($args);
	}elsif ($cmd eq 'login') {
		$o = &login($args);
		$cid = $o->{cid};
		$token = $o->{token};
	}elsif ($cmd eq 'logout') {
		&logout($args);
		undef $cid;
		undef $token;
	}elsif ($cmd eq 'talk') {
		$o = &talk($args);
	}elsif ($cmd eq 'move') {
		$o = &move($args);
	}elsif ($cmd eq 'attack') {
		$o = &attack($args);
	}elsif ($cmd eq 'startnpc') {
		$o = &startnpc($args);
	}else {
		$o = &get_list_to_know();
		print Dumper($o);
	}
}


sub call_api
{
	
	($url, %params) = @_ ;

	$ua = LWP::UserAgent->new;
	$req = POST $url, [%params];
	$res = $ua->request($req);
	if ($res->is_success) {
		print "content:" . $res->content . "\n";
		print "Dumper:" . Dumper(from_json($res->content)) . "\n";
		$o = from_json($res->content);
		return $o
	} else {
		print $res->status_line . "\n";
	}
}
sub subscribe
{
	($id, $pass) = split(/ /, shift, 2);
	my $target_url = "$base_url/service/$sv/subscribe/";
	my %params = ('id' => $id, 'password' => $pass);
	return &call_api($target_url, %params);
}
sub passwd
{
	($id, $pass, $newpass) = split(/ /, shift, 3);
	my $target_url = "$base_url/service/$sv/change_password/";
	my %params = ('id' => $id, 'password' => $pass, 'newpassword'=>$newpass);
	return &call_api($target_url, %params);
}
sub login
{
	($id, $pass) = split(/ /, shift, 2);
	my $target_url = "$base_url/service/$sv/login/";
	my %params = ('id' => $id, 'password' => $pass);
	return &call_api($target_url, %params);
}

sub logout
{
	my $target_url = "$base_url/service/$sv/logout/" . $cid;
	my %params = ('token' => $token);
	return &call_api($target_url, %params);
}

sub get_list_to_know
{
	my $target_url = "$base_url/service/$sv/listtoknow/" . $cid;
	my %params = ('token' => $token);
	return &call_api($target_url, %params);
}

sub talk
{
	$talked = shift;
	my $target_url = "$base_url/service/$sv/talk/$cid"; 
	my %params = ('token' => $token, 'talked' => $talked);
	return &call_api($target_url, %params);
}

sub move
{
	($x, $y) = split(/ /, shift, 2);
	my $target_url = "$base_url/service/$sv/move/$cid"; 
	my %params = ('token' => $token, 'x' => $x, 'y' => $y);
	return &call_api($target_url, %params);
}

sub attack
{
	$attack_to = shift;
	my $target_url = "$base_url/service/$sv/attack/$cid/$attack_to"; 
	my %params = ('token' => $token);
	return &call_api($target_url, %params);
}

sub startnpc
{
	$npcid = shift;
	my $target_url = "$base_url/service/$sv/startnpc/$npcid"; 
	my %params = ('token' => $token);
	return &call_api($target_url, %params);
}

1;
