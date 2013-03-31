{application, simpledungeon,
 [{vsn,"0.0.1"},
  {modules,
		[simpledungeon, battle_observer, task,
		battle, throw, unarmed, admin, character,
		character_stream, db, mmoasp, morningcall,
		mout, move, npc, path_finder, test,
		u, uauth, world, yaws_if]},

  {registered, [simpledungeon]},
  {mod, {simpledungeon, []}}
]}.
