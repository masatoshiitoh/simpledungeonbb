{application,simpledungeon,
 [{description,"simpledungeon MMO server"},
  {vsn,"0.01"},
  {modules,[simpledungeon, battle_observer, task, battle, throw, unarmed, admin, character, character_stream, db, mmoasp, morningcall, mout, move, npc, path_finder, test, u, uauth, world, yaws_if]},
  {registered, []},
  {mod,{simpledungeon,[]}},
  {env, [
         % {debug, false},                % true | false
         % {trace, false},                % http | traffic | false
         % {traceoutput, false},          % true | false
         % {conf, "/etc/yaws.conf"},      % string()
         % {runmod, mymodule},            % atom()
         % {embedded, false},             % true | false
         % {id, "default"},               % string()
         % {pam_service, "system-auth"},  % string()
         % {pam_use_acct, true},          % true | false
         % {pam_use_sess, true}           % true | false
        ]},
  {applications,[kernel,stdlib]}]}.
